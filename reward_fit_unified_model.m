function model = reward_fit_unified_model(RT, hit, Reward_Condition)
% FITS a 7-parameter psychometric model to forced RT data by max. likelihood.
% This model tests for the moderating effect of a binary condition (Reward vs No-Reward)
% on the mean time (mu), the upper asymptote (qA), AND the standard deviation (sigma).
%
% Inputs:
%       RT             - Combined vector of reaction times (in s).
%       hit            - Combined vector of responses (1 = hit, 0 = miss).
%       Reward_Condition - Input vector (e.g., 3=No-Reward, 4=Reward) matching RT/hit.
%       
% Outputs:
%       model.paramsOpt         - Optimized 7 parameters.
%       model.LLopt             - Optimized Negative Log-Likelihood (NLL).
%       model.nLL_init          - NLL for initial parameter guess.
%       model.p_hit_plot_sham   - Predicted P(Hit) curve for No-Reward condition (for plotting).
%       model.p_hit_plot_stim   - Predicted P(Hit) curve for Reward condition (for plotting).
%
% Model parametrization (7 elements):
% [p1=mu_noreward, p2=sigma_noreward, p3=qI (fixed), p4=qA_noreward, p5=Delta_mu, p6=Delta_qA, p7=Delta_sigma]
% -------------------------------------------------------------------------

% --- 1. Initialization and Parameter Bounds (Now 7 Elements) ---

% Initial parameters: [mu, sigma, qI, qA, Delta_mu, Delta_qA, Delta_sigma]
paramsInit = [0.1, 0.1, 0.25, 0.95, 0, 0, 0]; 

% Lower Bound: [mu, sigma, qI, qA, Delta_mu_min, Delta_qA_min, Delta_sigma_min]
% CRITICAL FIX: Delta_sigma_min must be > -sigma_noreward_min (0.01) to ensure sigma_reward remains positive.
LB = [0.0005, 0.01, 0.1, 0.5, -0.05, -0.1, -0.009]; % Adjusted Delta_sigma_min from -0.09

% Upper Bound: [mu, sigma, qI, qA, Delta_mu_max, Delta_qA_max, Delta_sigma_max]
UB = [0.5, 1, 1, 1, 0.05, 0.1, 0.1];

% Inequality constraints (A*params <= B)
% Only C1 remains: ensures qA_reward <= 1 (The physical probability limit).
% Constraint 1 (C1): qA_reward <= 1      ->  p(4) + p(6) <= 1
A = [0,  0, 0,  1, 0,  1,  0];   % C1: p4 + p6 <= 1
B = [1]; 

Aeq = []; Beq = []; % Equality constraints (none)

% --- 2. Data Cleaning and Filtering ---

% 2a. Weed out bad trials (NaNs)
good_trials = ~isnan(RT) & ~isnan(hit);
RT = RT(good_trials);
hit = hit(good_trials);
Reward_Condition = Reward_Condition(good_trials); 

% 2b. Filter out negative or extremely fast RTs (e.g., < 1ms)
valid_rt = RT > -1; 
RT = RT(valid_rt);
hit = hit(valid_rt);
Reward_Condition = Reward_Condition(valid_rt); 

% --- CRITICAL FIX: Convert 3/4 conditions to 0/1 binary indicator ---
% 0 = No Reward (Reward_Condition == 3) - This will be the baseline
% 1 = Reward (Reward_Condition == 4)    - This will be the delta effect
Binary_Condition = (Reward_Condition == 4); % Creates a logical (0 or 1) array


% --- 3. Define the Log-Likelihood Function (7 Parameters) ---
% The qI (p3) is now fixed across conditions. The sigma (p2) is varied by p7.

logLikelihood = @(params) ...
    ... % Define effective parameters based on the current 'params' vector
    -sum(hit .* log( params(3) + ...                                                        /* qI (fixed p3) */
        ( (params(4) + params(6) * Binary_Condition) - params(3) ) .* ...                      /* qA_eff - qI */
        normcdf(RT, (params(1) + params(5) * Binary_Condition), (params(2) + params(7) * Binary_Condition))) + ... /* mu_eff, sigma_eff */
    (1-hit) .* log(1 - params(3) + ...                                                      /* 1 - qI */
        ( params(3) - (params(4) + params(6) * Binary_Condition) ) .* ...                      /* qI - qA_eff */
        normcdf(RT, (params(1) + params(5) * Binary_Condition), (params(2) + params(7) * Binary_Condition))));    %/* mu_eff, sigma_eff */


% --- 4. Optimization (fmincon) ---

% Pass the A and B constraints to fmincon
[model.paramsOpt, model.LLopt] = fmincon(logLikelihood, paramsInit, A, B, Aeq, Beq, LB, UB);

% --- 5. Calculate Final Model Fits and Likelihoods ---

model.nLL_init = logLikelihood(paramsInit);
model.nLL = logLikelihood(model.paramsOpt); % negative log-likelihood for optimized model

% FIX: Extend plotting range to ensure the full asymptote is shown
model.t_plot = [0:.001:0.5]; % Adjusted range from 0 to 0.5s 

% Extract optimized parameters for plotting convenience
p_opt = model.paramsOpt;
mu_noreward = p_opt(1);
sigma_noreward = p_opt(2); % Baseline sigma
qI = p_opt(3); % Fixed qI
qA_noreward = p_opt(4);
Delta_mu = p_opt(5);
Delta_qA = p_opt(6);
Delta_sigma = p_opt(7); % NEW Delta sigma

% Calculate P_Hit for NO-REWARD (Condition = 0)
mu_noreward_eff = mu_noreward;
qA_noreward_eff = qA_noreward;
model.p_hit_plot_noreward = qI + (qA_noreward_eff - qI) * normcdf(model.t_plot, mu_noreward_eff, sigma_noreward);

% Calculate P_Hit for REWARD (Condition = 1)
mu_reward_eff = mu_noreward + Delta_mu;
sigma_reward_eff = sigma_noreward + Delta_sigma; % Effective sigma for reward
qA_reward_eff = qA_noreward + Delta_qA; 
model.p_hit_plot_reward = qI + (qA_reward_eff - qI) * normcdf(model.t_plot, mu_reward_eff, sigma_reward_eff);