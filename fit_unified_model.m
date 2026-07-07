function model = fit_unified_model(RT, hit, Stim_Condition)
% FITS a 7-parameter psychometric model to forced RT data by max. likelihood.
% This model tests for the moderating effect of a binary condition (e.g., Stim vs Sham)
% on the mean time (mu), the upper asymptote (qA), AND the standard deviation (sigma).
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
% [p1=mu_sham, p2=sigma_sham, p3=qI, p4=qA_sham, p5=Delta_mu, p6=Delta_qA, p7=Delta_sigma]
% -------------------------------------------------------------------------

% --- 1. Initialization and Parameter Bounds (Now 7 Elements) ---

% Initial parameters: [mu, sigma, qI, qA, Delta_mu, Delta_qA, Delta_sigma]
paramsInit = [0.1, 0.1, 0.25, 0.95, 0, 0, 0]; 

% Lower Bound: [mu, sigma, qI, qA, Delta_mu_min, Delta_qA_min, Delta_sigma_min]
% Delta_sigma_min must prevent sigma_stim from becoming non-positive.
LB = [0.05, 0.03, 0.1, 0.5, -0.1, -0.1, -0.01]; 

% Upper Bound: [mu, sigma, qI, qA, Delta_mu_max, Delta_qA_max, Delta_sigma_max]
UB = [0.5, 1, 1, 1, 0.1, 0.1, 0.1];

% Inequality constraints (A*params <= B)
% Constraint 1 (C1): qA_stim <= 1      ->  p(4) + p(6) <= 1
% Constraint 2 (C2): qI < qA_sham      ->  p(3) - p(4) <= 0
% Constraint 3 (C3): qI < qA_stim      ->  p(3) - p(4) - p(6) <= 0
% Constraint 4 (C4): sigma_stim > 0.001 -> -p(2) - p(7) <= -0.001
A = [0,  0, 0,  1, 0,  1,  0;   % C1: p4 + p6 <= 1
     0,  0, 1, -1, 0,  0,  0;   % C2: p3 - p4 <= 0
     0,  0, 1, -1, 0, -1,  0;   % C3: p3 - p4 - p6 <= 0
     0, -1, 0,  0, 0,  0, -1];  % C4: -p2 - p7 <= -0.001
B = [1; 
     0; 
     0;
     -0.001];

Aeq = []; Beq = []; % Equality constraints (none)

% --- 2. Data Cleaning and Filtering ---

% 2a. Weed out bad trials (NaNs)
good_trials = ~isnan(RT);
RT = RT(good_trials);
hit = hit(good_trials);
Stim_Condition = Stim_Condition(good_trials); 

% 2b. Filter out negative or extremely fast RTs (e.g., < 1ms)
valid_rt = RT > 0.001; 
RT = RT(valid_rt);
hit = hit(valid_rt);
Stim_Condition = Stim_Condition(valid_rt); 

% --- 3. Define the Log-Likelihood Function (7 Parameters) ---
% Note: qI (p3) is fixed across conditions.

logLikelihood = @(params) ...
    ... % Define effective parameters based on the current 'params' vector
    -sum(hit .* log( params(3) + ...                                                        /* qI_eff (p3) */
        ( (params(4) + params(6) * Stim_Condition) - params(3) ) .* ...                      /* qA_eff - qI_eff */
        normcdf(RT, (params(1) + params(5) * Stim_Condition), (params(2) + params(7) * Stim_Condition))) + ... /* mu_eff, sigma_eff */
    (1-hit) .* log(1 - params(3) + ...                                                      /* 1 - qI_eff */
        ( params(3) - (params(4) + params(6) * Stim_Condition) ) .* ...                      /* qI_eff - qA_eff */
        normcdf(RT, (params(1) + params(5) * Stim_Condition), (params(2) + params(7) * Stim_Condition))));    %/* mu_eff, sigma_eff */

% --- 4. Optimization (fmincon) ---

[model.paramsOpt, model.LLopt] = fmincon(logLikelihood, paramsInit, A, B, Aeq, Beq, LB, UB);

% --- 5. Calculate Final Model Fits and Likelihoods ---

model.nLL_init = logLikelihood(paramsInit);
model.nLL = logLikelihood(model.paramsOpt); % negative log-likelihood for optimized model

% FIX: Extend plotting range
model.t_plot = [0:.001:0.5]; 

% Extract optimized parameters for plotting convenience
p_opt = model.paramsOpt;
mu_sham = p_opt(1);
sigma_sham = p_opt(2);
qI = p_opt(3);
qA_sham = p_opt(4);
Delta_mu = p_opt(5);
Delta_qA = p_opt(6);
Delta_sigma = p_opt(7); % NEW Parameter

% Calculate P_Hit for SHAM (Condition = 0)
mu_sham_eff = mu_sham;
sigma_sham_eff = sigma_sham;
qA_sham_eff = qA_sham;
model.p_hit_plot_sham = qI + (qA_sham_eff - qI) * normcdf(model.t_plot, mu_sham_eff, sigma_sham_eff);

% Calculate P_Hit for STIMULATION (Condition = 1)
mu_stim_eff = mu_sham + Delta_mu;
sigma_stim_eff = sigma_sham + Delta_sigma; % NEW effective sigma
qA_stim_eff = qA_sham + Delta_qA; 
model.p_hit_plot_stim = qI + (qA_stim_eff - qI) * normcdf(model.t_plot, mu_stim_eff, sigma_stim_eff);