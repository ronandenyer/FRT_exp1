function model = psychometric_2x2_model(RT, hit, Stim_Condition, Reward_Condition)
% FITS a 13-parameter psychometric model to forced RT data for a 2x2 design.
% This model tests for the moderating effects of tVNS and Reward across 4 quadrants.
%
% Inputs:
%       RT               - Combined vector of reaction times (in s).
%       hit              - Combined vector of responses (1 = hit, 0 = miss).
%       Stim_Condition   - Vector coding stimulation (e.g., 1=Sham, 2=tVNS)
%       Reward_Condition - Vector coding reward (e.g., 3=No-Reward, 4=Reward)
%       
% Outputs:
%       model.paramsOpt  - Optimized 13 parameters.
%       model.params_c*  - Effective [mu, sigma, qI, qA] calculated for each quadrant.
%       model.plot_*     - Predicted P(Hit) curves for plotting each quadrant.
%
% Model parametrization (13 elements):
% Global: p3 = qI (fixed lapse rate at t=0)
% Baseline (Sham + No-Rew): p1=mu, p2=sigma, p4=qA
% Deltas for Sham+Rew:      p5=D_mu, p6=D_qA, p7=D_sigma
% Deltas for tVNS+No-Rew:   p8=D_mu, p9=D_qA, p10=D_sigma
% Deltas for tVNS+Rew:      p11=D_mu, p12=D_qA, p13=D_sigma
% -------------------------------------------------------------------------

% --- 1. Initialization and Parameter Bounds ---
% Initial parameters: [mu, sig, qI, qA, Dmu2, DqA2, Dsig2, Dmu3, DqA3, Dsig3, Dmu4, DqA4, Dsig4]
paramsInit = [0.1, 0.1, 0.25, 0.95, 0, 0, 0, 0, 0, 0, 0, 0, 0]; 

% Lower Bound: prevents standard deviation deltas from crossing below 0
LB = [0.0005, 0.01, 0.1, 0.5, -0.05, -0.1, -0.009, -0.05, -0.1, -0.009, -0.05, -0.1, -0.009]; 

% Upper Bound
UB = [0.5, 1, 1, 1, 0.05, 0.1, 0.1, 0.05, 0.1, 0.1, 0.05, 0.1, 0.1];

% Inequality constraints (A*params <= B)
% Ensure qA_eff <= 1 (Probability limit) for all 3 experimental conditions
A = [];
B = []; 

Aeq = []; Beq = []; % No equality constraints

% --- 2. Data Cleaning and Filtering ---
good_trials = ~isnan(RT) & ~isnan(hit) & ~isnan(Stim_Condition) & ~isnan(Reward_Condition);
RT = RT(good_trials);
hit = hit(good_trials);
Stim_Condition = Stim_Condition(good_trials);
Reward_Condition = Reward_Condition(good_trials);

valid_rt = RT > -1; 
RT = RT(valid_rt);
hit = hit(valid_rt);
Stim_Condition = Stim_Condition(valid_rt);
Reward_Condition = Reward_Condition(valid_rt);

% --- 3. Safely Map Conditions to Binary Indicators ---
% Automatically handles arbitrary raw codes (0/1, 1/2, or 3/4). 
% Assumes the maximum numerical value represents the active manipulation.
is_Reward = (Reward_Condition == max(Reward_Condition));
is_tVNS   = (Stim_Condition == max(Stim_Condition));

C2 = (~is_tVNS & is_Reward);  % Condition 2: Sham + Reward
C3 = (is_tVNS & ~is_Reward);  % Condition 3: tVNS + No Reward
C4 = (is_tVNS & is_Reward);   % Condition 4: tVNS + Reward

% --- 4. Define the Log-Likelihood Function (13 Parameters) ---
logLikelihood = @(p) ...
    -sum(hit .* log( p(3) + ...
        ( (p(4) + p(6)*C2 + p(9)*C3 + p(12)*C4) - p(3) ) .* ...
        normcdf(RT, (p(1) + p(5)*C2 + p(8)*C3 + p(11)*C4), (p(2) + p(7)*C2 + p(10)*C3 + p(13)*C4))) + ...
    (1-hit) .* log(1 - p(3) + ...
        ( p(3) - (p(4) + p(6)*C2 + p(9)*C3 + p(12)*C4) ) .* ...
        normcdf(RT, (p(1) + p(5)*C2 + p(8)*C3 + p(11)*C4), (p(2) + p(7)*C2 + p(10)*C3 + p(13)*C4))));

% --- 5. Optimization (fmincon) ---
% Suppress console output for faster looping across subjects
options = optimoptions('fmincon', 'Display', 'off');
[model.paramsOpt, model.LLopt] = fmincon(logLikelihood, paramsInit, A, B, Aeq, Beq, LB, UB, [], options);

% --- 6. Calculate Final Model Fits and Store Effective Parameters ---
model.nLL_init = logLikelihood(paramsInit);
model.nLL = logLikelihood(model.paramsOpt); 

model.t_plot = [0:.001:0.5]; 
p = model.paramsOpt;

% Package the effective final parameters [mu, sigma, qI, qA] for each quadrant 
% so you don't have to calculate the math manually later.
model.params_sham_noreward = [p(1),       p(2),       p(3), p(4)];
model.params_sham_reward   = [p(1)+p(5),  p(2)+p(7),  p(3), p(4)+p(6)];
model.params_tvns_noreward = [p(1)+p(8),  p(2)+p(10), p(3), p(4)+p(9)];
model.params_tvns_reward   = [p(1)+p(11), p(2)+p(13), p(3), p(4)+p(12)];

% Generate predicted P(Hit) curves for plotting
model.plot_sham_noreward = p(3) + (model.params_sham_noreward(4) - p(3)) * normcdf(model.t_plot, model.params_sham_noreward(1), model.params_sham_noreward(2));
model.plot_sham_reward   = p(3) + (model.params_sham_reward(4) - p(3)) * normcdf(model.t_plot, model.params_sham_reward(1), model.params_sham_reward(2));
model.plot_tvns_noreward = p(3) + (model.params_tvns_noreward(4) - p(3)) * normcdf(model.t_plot, model.params_tvns_noreward(1), model.params_tvns_noreward(2));
model.plot_tvns_reward   = p(3) + (model.params_tvns_reward(4) - p(3)) * normcdf(model.t_plot, model.params_tvns_reward(1), model.params_tvns_reward(2));

end