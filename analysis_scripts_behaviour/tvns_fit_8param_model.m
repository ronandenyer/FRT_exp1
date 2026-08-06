function model = tvns_fit_8param_model(RT, hit, Stim_Condition)
% FITS an 8-parameter psychometric model to forced RT data.
% Moderating effects on: mu, sigma, qA (upper), and qI (lower).
%
% Model parametrization (8 elements):
% [p1=mu_sham, p2=sigma_sham, p3=qI_sham, p4=qA_sham, ...
%  p5=Delta_mu, p6=Delta_qA, p7=Delta_sigma, p8=Delta_qI]
% --- 1. Initialization and Parameter Bounds ---
% [mu, s, qI, qA, D_mu, D_qA, D_s, D_qI]
paramsInit = [0.1, 0.01, 0.25, 0.95, 0, 0, 0, 0]; 

% Lower Bounds
% Note: p8 (D_qI) min set to -0.1 to ensure qI_eff stays above ~0.15
LB = [0.0001, 0.001, 0.1, 0.5, -0.05, -0.5, -0.1, -0.2]; 
% Upper Bounds
UB = [0.4,  0.5,   0.4, 1,  0.05,  0.5,  0.5,   0.2];

% --- 2. Linear Inequality Constraints (A*p <= B) ---
% C1: qA_stim <= 1          ->  p(4) + p(6) <= 1
% C2: qI_stim >= 0          -> -p(3) - p(8) <= 0
% C3: qI_sham < qA_sham     ->  p(3) - p(4) <= -0.01 (Prevents vertical lines)
% C4: qI_stim < qA_stim     ->  (p3+p8) - (p4+p6) <= -0.01
% C5: sigma_stim > 0.001    -> -p(2) - p(7) <= -0.001 (Prevents Z-shape)

A = [ 0,  0,  0,  1,  0,  1,  0,  0;  % C1
      0,  0,  0,  0,  0,  0,  0, -1;  % C2
      0,  0,  1, -1,  0,  0,  0,  0;  % C3
      0,  0,  1, -1,  0, -1,  0,  1;  % C4
      0, -1,  0,  0,  0,  0, -1,  0]; % C5
      
B = [ 1; 
      0; 
     -0.01; 
     -0.01; 
     -0.001];

Aeq = []; Beq = [];

% --- 3. Data Cleaning ---
good_trials = ~isnan(RT) & ~isnan(hit);
RT = RT(good_trials);
hit = hit(good_trials);
Stim_Condition = Stim_Condition(good_trials); 
Binary_Condition = (Stim_Condition == 2); 

% --- 4. Define the Log-Likelihood Function ---
logLikelihood = @(params) ...
    -sum(hit .* log( (params(3) + params(8) * Binary_Condition) + ...                     /* qI_eff */
        ( (params(4) + params(6) * Binary_Condition) - (params(3) + params(8) * Binary_Condition) ) .* ... /* qA_eff - qI_eff */
        normcdf(RT, (params(1) + params(5) * Binary_Condition), (params(2) + params(7) * Binary_Condition))) + ... /* mu_eff, sigma_eff */
    (1-hit) .* log(1 - ( (params(3) + params(8) * Binary_Condition) + ...                /* 1 - qI_eff */
        ( (params(4) + params(6) * Binary_Condition) - (params(3) + params(8) * Binary_Condition) ) .* ... /* qA_eff - qI_eff */
        normcdf(RT, (params(1) + params(5) * Binary_Condition), (params(2) + params(7) * Binary_Condition))))); 

% --- 5. Optimization ---
options = optimoptions('fmincon','Display','off','Algorithm','sqp');
[model.paramsOpt, model.LLopt] = fmincon(logLikelihood, paramsInit, A, B, Aeq, Beq, LB, UB, [], options);

% --- 6. Plotting Curves ---
model.t_plot = 0:.001:0.5;
p = model.paramsOpt;

% Sham Curve
model.p_hit_plot_sham = p(3) + (p(4) - p(3)) * normcdf(model.t_plot, p(1), p(2));

% Stim Curve
qI_stim = p(3) + p(8);
qA_stim = p(4) + p(6);
mu_stim = p(1) + p(5);
si_stim = p(2) + p(7);
model.p_hit_plot_stim = qI_stim + (qA_stim - qI_stim) * normcdf(model.t_plot, mu_stim, si_stim);

end