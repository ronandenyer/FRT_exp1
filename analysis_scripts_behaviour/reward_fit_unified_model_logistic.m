function model = reward_fit_unified_model_logistic(RT, hit, Reward_Condition)
% FITS an 8-parameter Logistic psychometric model to forced RT data.
% Includes Deltas for: Mean (mu), Scale (s), Upper Asymptote (qA), and Lower Asymptote (qI).
%
% Model parametrization (8 elements):
% [p1=mu_base, p2=s_base, p3=qI_base, p4=qA_base, p5=D_mu, p6=D_qA, p7=D_s, p8=D_qI]

% --- 1. Initialization and Parameter Bounds ---
% [mu, s, qI, qA, D_mu, D_qA, D_s, D_qI]
paramsInit = [0.1, 0.01, 0.25, 0.95, 0, 0, 0, 0]; 

% Lower Bounds
% Note: p8 (D_qI) min set to -0.1 to ensure qI_eff stays above ~0.15
LB = [0.05, 0.001, 0.1, 0.5, -0.05, -0.5, -0.0009, -0.1]; 
% Upper Bounds
UB = [0.4,  0.5,   0.4, 1,  0.05,  0.5,  0.5,   0.2];

% Inequality constraints (A*params <= B)
% C1: qA_base + D_qA <= 1  -> [0 0 0 1 0 1 0 0]
% C2: qI_base + D_qI >= 0  -> [0 0 -1 0 0 0 0 -1] <= 0
A = [0, 0,  0, 1, 0, 1, 0,  0;
     0, 0, -1, 0, 0, 0, 0, -1];   
B = [1; 0]; 

% --- 2. Data Cleaning ---
good_trials = ~isnan(RT) & ~isnan(hit);
RT = RT(good_trials);
hit = hit(good_trials);
Binary_Condition = (Reward_Condition(good_trials) == 4); 

% --- 3. Define the Logistic Log-Likelihood Function ---
logistic_func = @(t, m, s) 1 ./ (1 + exp(-(t - m) ./ s));

logLikelihood = @(p) ...
    -sum(hit .* log( (p(3) + p(8)*Binary_Condition) + ...                        /* qI_eff */
        ((p(4) + p(6)*Binary_Condition) - (p(3) + p(8)*Binary_Condition)) .* ... /* qA_eff - qI_eff */
        logistic_func(RT, (p(1) + p(5)*Binary_Condition), (p(2) + p(7)*Binary_Condition))) + ... 
    (1-hit) .* log(1 - ((p(3) + p(8)*Binary_Condition) + ...                     /* 1 - qI_eff */
        ((p(4) + p(6)*Binary_Condition) - (p(3) + p(8)*Binary_Condition)) .* ... 
        logistic_func(RT, (p(1) + p(5)*Binary_Condition), (p(2) + p(7)*Binary_Condition)))));

% --- 4. Optimization ---
options = optimoptions('fmincon','Display','off','MaxFunctionEvaluations', 5000);
[model.paramsOpt, model.LLopt] = fmincon(logLikelihood, paramsInit, A, B, [], [], LB, UB, [], options);

% --- 5. Final Model Fits for Plotting ---
model.t_plot = [0:.001:0.5]; 
p = model.paramsOpt;

% Baseline (No Reward)
qI_0 = p(3); qA_0 = p(4);
model.p_hit_plot_sham = qI_0 + (qA_0 - qI_0) * logistic_func(model.t_plot, p(1), p(2));

% Reward (Reward)
qI_1 = p(3) + p(8); qA_1 = p(4) + p(6);
mu_1 = p(1) + p(5); s_1  = p(2) + p(7);
model.p_hit_plot_stim = qI_1 + (qA_1 - qI_1) * logistic_func(model.t_plot, mu_1, s_1);

model.nLL = model.LLopt;
end