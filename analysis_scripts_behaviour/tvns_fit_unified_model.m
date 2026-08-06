function model = tvns_fit_unified_model(RT, hit, Stim_Condition)
% FITS a 7-parameter psychometric model to forced RT data by max. likelihood.
% This model tests for the moderating effect of a binary condition (Stimulation vs Sham)
% on the mean time (mu), the upper asymptote (qA), AND the standard deviation (sigma).
%
% Inputs:
%       RT             - Combined vector of reaction times (in s).
%       hit            - Combined vector of responses (1 = hit, 0 = miss).
%       Stim_Condition - Input vector (e.g., 1=Sham/Control, 2=Stimulation) matching RT/hit.
%       
% Outputs:
%       model.paramsOpt         - Optimized 7 parameters.
%       model.LLopt             - Optimized Negative Log-Likelihood (NLL).
%       model.nLL_init          - NLL for initial parameter guess.
%       model.p_hit_plot_sham   - Predicted P(Hit) curve for Sham condition (for plotting).
%       model.p_hit_plot_stim   - Predicted P(Hit) curve for Stimulation condition (for plotting).
%
% Model parametrization (7 elements):
% [p1=mu_sham, p2=sigma_sham, p3=qI (fixed), p4=qA_sham, p5=Delta_mu, p6=Delta_qA, p7=Delta_sigma]
% -------------------------------------------------------------------------

% --- 1. Initialization and Parameter Bounds (Now 7 Elements) ---

% Initial parameters: [mu, sigma, qI, qA, Delta_mu, Delta_qA, Delta_sigma]
paramsInit = [0.1, 0.1, 0.25, 0.95, 0, 0, 0]; 

% Lower Bound: [mu, sigma, qI, qA, Delta_mu_min, Delta_qA_min, Delta_sigma_min]
% CRITICAL FIX: Delta_sigma_min must be > -sigma_sham_min (0.01) to ensure sigma_stim remains positive.
LB = [0.00001, 0.01, 0.1, 0.5, -0.2, -0.5, -0.1]; 

% Upper Bound: [mu, sigma, qI, qA, Delta_mu_max, Delta_qA_max, Delta_sigma_max]
UB = [0.5, 1, 1, 1, 0.2, 0.5, 0.5];

% Inequality constraints (A*params <= B)
% Only C1 remains: ensures qA_stim <= 1 (The physical probability limit).
% Constraint 1 (C1): qA_stim <= 1      ->  p(4) + p(6) <= 1
A = [0,  0, 0,  1, 0,  1,  0];   % C1: p4 + p6 <= 1
B = [1]; 

Aeq = []; Beq = []; % Equality constraints (none)

% --- 2. Data Cleaning and Filtering ---

% 2a. Weed out bad trials (NaNs)
good_trials = ~isnan(RT) & ~isnan(hit);
RT = RT(good_trials);
hit = hit(good_trials);
Stim_Condition = Stim_Condition(good_trials); 

% 2b. Filter out negative or extremely fast RTs (e.g., < 1ms)
% valid_rt = RT > 0.001; 
% RT = RT(valid_rt);
% hit = hit(valid_rt);
% Stim_Condition = Stim_Condition(valid_rt); 

% --- CRITICAL FIX: Convert 1/2 conditions to 0/1 binary indicator ---
% 0 = Sham (Stim_Condition == 1) - This will be the baseline
% 1 = Stimulation (Stim_Condition == 2) - This will be the delta effect
Binary_Condition = (Stim_Condition == 2); % Creates a logical (0 or 1) array


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
model.t_plot = [0:.001:0.5]; % Range set to 0 to 0.5s 

% Extract optimized parameters for plotting convenience
p_opt = model.paramsOpt;
mu_sham = p_opt(1);
sigma_sham = p_opt(2); % Baseline sigma
qI = p_opt(3); % Fixed qI
qA_sham = p_opt(4);
Delta_mu = p_opt(5);
Delta_qA = p_opt(6);
Delta_sigma = p_opt(7); % Delta sigma

% Calculate P_Hit for SHAM (Condition = 0)
mu_sham_eff = mu_sham;
qA_sham_eff = qA_sham;
model.p_hit_plot_sham = qI + (qA_sham_eff - qI) * normcdf(model.t_plot, mu_sham_eff, sigma_sham);

% Calculate P_Hit for STIMULATION (Condition = 1)
mu_stim_eff = mu_sham + Delta_mu;
sigma_stim_eff = sigma_sham + Delta_sigma; % Effective sigma for stimulation
qA_stim_eff = qA_sham + Delta_qA; 
model.p_hit_plot_stim = qI + (qA_stim_eff - qI) * normcdf(model.t_plot, mu_stim_eff, sigma_stim_eff);