function model = fit_unified_model(RT, hit, Stim_Condition)
% FITS a 7-parameter psychometric model to forced RT data by max. likelihood.
% This model tests for the moderating effect of a binary condition (e.g., Stim vs Sham)
% on the mean time (mu), the upper asymptote (qA), AND the lower asymptote (qI).
%
% Inputs:
%       RT             - Combined vector of reaction times (in s).
%       hit            - Combined vector of responses (1 = hit, 0 = miss).
%       Stim_Condition - Binary vector (0=Sham/Control, 1=Stimulation) matching RT/hit.
%       
% Outputs:
%       model.paramsOpt         - Optimized 7 parameters.
%       model.LLopt             - Optimized Negative Log-Likelihood (NLL).
%       model.nLL_init          - NLL for initial parameter guess.
%       model.p_hit_plot_sham   - Predicted P(Hit) curve for Sham condition (for plotting).
%       model.p_hit_plot_stim   - Predicted P(Hit) curve for Stim condition (for plotting).
%
% Model parametrization (7 elements):
% [p1=mu_sham, p2=sigma, p3=qI_sham, p4=qA_sham, p5=Delta_mu, p6=Delta_qA, p7=Delta_qI]
% -------------------------------------------------------------------------

% --- 1. Initialization and Parameter Bounds (Now 7 Elements) ---

% Initial parameters: [mu, sigma, qI, qA, Delta_mu, Delta_qA, Delta_qI]
paramsInit = [0.1, 0.1, 0.25, 0.95, 0, 0, 0]; 

% Lower Bound: [mu, sigma, qI, qA, Delta_mu_min, Delta_qA_min, Delta_qI_min]
% Allowing Delta_qI to drop 0.15 below the initial qI bound of 0.1
LB = [0.005, 0.01, 0.2, 0.6, -0.2, -0.2, -0.2]; 

% Upper Bound: [mu, sigma, qI, qA, Delta_mu_max, Delta_qA_max, Delta_qI_max]
UB = [0.5, 1, 0.3, 1, 0.2, 0.2, 0.2];

% Inequality constraints (A*params <= B)
% Constraint 1: qA_stim <= 1      ->  p(4) + p(6) <= 1  (Probability constraint)
% Constraint 2: qI_sham < qA_sham ->  p(3) - p(4) <= 0  (Structural constraint for Sham)
% Constraint 3: qI_stim < qA_stim ->  p(3) + p(7) - p(4) - p(6) <= 0 (Structural constraint for Stim)
A = [0, 0, 0,  1, 0,  1,  0;   % C1: p4 + p6 <= 1
     0, 0, 1, -1, 0,  0,  0;   % C2: p3 - p4 <= 0
     0, 0, 1, -1, 0, -1,  1];  % C3: p3 - p4 - p6 + p7 <= 0
B = [1; 
     0; 
     0];

Aeq = []; Beq = []; % Equality constraints (none)

% --- 2. Data Cleaning and Filtering ---

% 2a. Weed out bad trials (NaNs)
good_trials = ~isnan(RT) & ~isnan(hit);
RT = RT(good_trials);
hit = hit(good_trials);
Stim_Condition = Stim_Condition(good_trials); 

% 2b. Filter out negative or extremely fast RTs (e.g., < 1ms)
valid_rt = RT > -0.1; 
RT = RT(valid_rt);
hit = hit(valid_rt);
Stim_Condition = Stim_Condition(valid_rt); 

% --- 3. Define the Log-Likelihood Function (7 Parameters) ---
% All effective parameter calculations are INLINED here to avoid scoping errors.

logLikelihood = @(params) ...
    ... % Define effective parameters based on the current 'params' vector
    -sum(hit .* log( (params(3) + params(7) * Stim_Condition) + ...                                  /* qI_eff */
        ( (params(4) + params(6) * Stim_Condition) - (params(3) + params(7) * Stim_Condition) ) .* ... /* qA_eff - qI_eff */
        normcdf(RT, (params(1) + params(5) * Stim_Condition), params(2))) + ...                       /* mu_eff, sigma */
    (1-hit) .* log(1 - (params(3) + params(7) * Stim_Condition) + ...                                 /* 1 - qI_eff */
        ( (params(3) + params(7) * Stim_Condition) - (params(4) + params(6) * Stim_Condition) ) .* ... /* qI_eff - qA_eff */
        normcdf(RT, (params(1) + params(5) * Stim_Condition), params(2))));                            %/* mu_eff, sigma */

% --- 4. Optimization (fmincon) ---

% Pass the A and B constraints to fmincon
[model.paramsOpt, model.LLopt] = fmincon(logLikelihood, paramsInit, A, B, Aeq, Beq, LB, UB);

% --- 5. Calculate Final Model Fits and Likelihoods ---

model.nLL_init = logLikelihood(paramsInit);
model.nLL = logLikelihood(model.paramsOpt); % negative log-likelihood for optimized model

% FIX: Extend plotting range to ensure the full asymptote is shown
model.t_plot = [0:.001:0.5]; % Extended range to 1.0s 

% Extract optimized parameters for plotting convenience
p_opt = model.paramsOpt;
mu_sham = p_opt(1);
sigma = p_opt(2);
qI_sham = p_opt(3);
qA_sham = p_opt(4);
Delta_mu = p_opt(5);
Delta_qA = p_opt(6);
Delta_qI = p_opt(7); % NEW Parameter

% Calculate P_Hit for SHAM (Condition = 0)
mu_sham_eff = mu_sham;
qI_sham_eff = qI_sham;
qA_sham_eff = qA_sham;
model.p_hit_plot_sham = qI_sham_eff + (qA_sham_eff - qI_sham_eff) * normcdf(model.t_plot, mu_sham_eff, sigma);

% Calculate P_Hit for STIMULATION (Condition = 1)
mu_stim_eff = mu_sham + Delta_mu;
qI_stim_eff = qI_sham + Delta_qI; % NEW effective qI
qA_stim_eff = qA_sham + Delta_qA; 
model.p_hit_plot_stim = qI_stim_eff + (qA_stim_eff - qI_stim_eff) * normcdf(model.t_plot, mu_stim_eff, sigma);