function model = fit_sequential_unified_model(RT, hit, Stim_Condition)
% FITS a 7-parameter unified model to a subset of data (e.g., Post-Error).
% p = [mu_sham, sigma_sham, qI (fixed), qA_sham, Delta_mu, Delta_qA, Delta_sigma]

% --- 1. Initialization and Bounds ---
paramsInit = [0.15, 0.05, 0.25, 0.95, 0, 0, 0]; 

% Lower Bound: mu, sig, qI, qA, D_mu, D_qA, D_sig
LB = [0.005, 0.005, 0.1, 0.5, -0.2, -0.5, -0.05]; 
UB = [0.5,   0.5,   0.5, 1.0,  0.2,  0.5,  0.5];

% --- 2. Robust Inequality Constraints ---
% C1: qA_stim <= 1      ->  p(4) + p(6) <= 1
% C2: qI < qA_sham      ->  p(3) - p(4) <= -0.01
% C3: qI < qA_stim      ->  p(3) - p(4) - p(6) <= -0.01
% C4: sigma_stim > 0.001 -> -p(2) - p(7) <= -0.001 (CRITICAL FIX)

A = [ 0,  0,  0,  1,  0,  1,  0;   % C1
      0,  0,  1, -1,  0,  0,  0;   % C2
      0,  0,  1, -1,  0, -1,  0;   % C3
      0, -1,  0,  0,  0,  0, -1];  % C4
B = [ 1; -0.01; -0.01; -0.001];

Aeq = []; Beq = [];

% --- 3. Data Cleaning ---
good_trials = ~isnan(RT) & ~isnan(hit);
RT = RT(good_trials);
hit = hit(good_trials);
Binary_Condition = (Stim_Condition == 2); 

% --- 4. Log-Likelihood ---
logLikelihood = @(params) ...
    -sum(hit .* log( params(3) + ...                                                        
        ( (params(4) + params(6) * Binary_Condition) - params(3) ) .* ...                      
        normcdf(RT, (params(1) + params(5) * Binary_Condition), (params(2) + params(7) * Binary_Condition))) + ... 
    (1-hit) .* log(1 - params(3) + ...                                                      
        ( params(3) - (params(4) + params(6) * Binary_Condition) ) .* ...                      
        normcdf(RT, (params(1) + params(5) * Binary_Condition), (params(2) + params(7) * Binary_Condition))));

% --- 5. Optimization ---
options = optimoptions('fmincon','Display','off','Algorithm','sqp');
[model.paramsOpt, model.LLopt] = fmincon(logLikelihood, paramsInit, A, B, Aeq, Beq, LB, UB, [], options);

% --- 6. Results ---
p = model.paramsOpt;
model.t_plot = 0:.001:0.5;
model.p_hit_plot_sham = p(3) + (p(4) - p(3)) * normcdf(model.t_plot, p(1), p(2));
model.p_hit_plot_stim = p(3) + ((p(4)+p(6)) - p(3)) * normcdf(model.t_plot, p(1)+p(5), p(2)+p(7));

end