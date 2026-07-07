% 1. Load the data
shooting_data = readtable('C://Users//denyer//OneDrive - UCL//FRT_data//tVNS_experiment//all_pats_tvns_hit_data.csv');
subject_list = unique(shooting_data.SID);

% 2. Prepare storage
all_true = [];
all_recovered = [];

% 3. Iterate through each real subject's trial structure
for s = 1:length(subject_list)
    % Use curly braces {} to extract the content of the cell (the string ID)
    current_sid = subject_list{s}; 
    
    % Use strcmp instead of ==
    % This creates a logical mask for the rows that match the current ID
    row_mask = strcmp(shooting_data.SID, current_sid);
    
    % Apply the mask
    sid_eff_pt = shooting_data.eff_pt(row_mask) / 1000;
    sid_eff_pt = sid_eff_pt(sid_eff_pt > 0 & ~isnan(sid_eff_pt));
    
    % Run a few simulations per subject to see if the model 
    % is stable for THIS specific person's trial distribution
    for iter = 1:5 
        % Use your "True" parameters
        p_hit = true_qI + (true_qA - true_qI) * normcdf(sid_eff_pt, true_mu, true_sigma);
        hit_sim = rand(length(sid_eff_pt), 1) < p_hit;
        
        % Fit using their specific trial structure
        model_sim = fit_model_MultiStart(sid_eff_pt, hit_sim);
        
        % Store
        all_true = [all_true; [true_mu, true_sigma, true_qI, true_qA]];
        all_recovered = [all_recovered; model_sim.paramsOpt];
    end
end
%% Viz
% Ensure we have the data
% all_true = [n_iter x 4] matrix
% all_recovered = [n_iter x 4] matrix

figure('Color', 'w', 'Position', [100, 100, 1000, 800]);
param_names = {'Mu', 'Sigma', 'qI', 'qA'};

for p = 1:4
    subplot(2, 2, p); hold on;
    
    % Extract current parameter data
    x = all_true(:, p);
    y = all_recovered(:, p);
    
    % Calculate Pearson correlation
    R = corrcoef(x, y);
    r_val = R(1, 2);
    
    % Plot scatter with transparency
    scatter(x, y, 40, 'filled', 'MarkerFaceAlpha', 0.5, 'MarkerFaceColor', [0.2 0.5 0.8]);
    
    % Draw identity line (perfect recovery)
    lims = [min([x; y]), max([x; y])];
    plot(lims, lims, 'r--', 'LineWidth', 2);
    
    % Labels and formatting
    title(sprintf('%s (r = %.3f)', param_names{p}, r_val));
    xlabel('True Value');
    ylabel('Recovered Value');
    grid on;
    axis square;
end

%% --- Grid Setup ---
% Define the ranges you want to test
mu_range = [0.1, 0.2, 0.3];        % Varying Mean (RT threshold)
sigma_range = [0.01, 0.025, 0.05]; % Varying Sigma (Sensitivity/Variance)
n_iter_per_grid = 10;              % Simulations per grid point

% Load real data to get trial structure
shooting_data = readtable('C://Users//denyer//OneDrive - UCL//FRT_data//tVNS_experiment//all_pats_tvns_hit_data.csv');
clean_eff_pt = shooting_data.eff_pt / 1000;
clean_eff_pt = clean_eff_pt(clean_eff_pt > 0 & ~isnan(clean_eff_pt));

% Create the grid
[MU_grid, SIG_grid] = meshgrid(mu_range, sigma_range);
grid_points = [MU_grid(:), SIG_grid(:)];

% Prepare results storage
% Format: [TrueMu, TrueSig, RecMu, RecSig]
results = []; 

fprintf('Starting Grid Search Recovery...\n');

for g = 1:size(grid_points, 1)
    true_mu = grid_points(g, 1);
    true_sigma = grid_points(g, 2);
    fprintf('Testing Grid Point: Mu=%.2f, Sigma=%.3f\n', true_mu, true_sigma);
    
    for i = 1:n_iter_per_grid
        % Use a random subset of trials for each iteration to mimic individual subjects
        RT_sample = randsample(clean_eff_pt, 500); 
        
        % 1. Simulate
        p_hit = 0.25 + (0.95 - 0.25) * normcdf(RT_sample, true_mu, true_sigma);
        hit_sim = rand(length(RT_sample), 1) < p_hit;
        
        % 2. Fit (using your function)
        model_sim = fit_model_MultiStart(RT_sample, hit_sim);
        
        % 3. Collect
        results = [results; true_mu, true_sigma, model_sim.paramsOpt(1), model_sim.paramsOpt(2)];
    end
end

% --- Visualization: The "Identifiability Heatmap" ---
figure('Color', 'w', 'Position', [100, 100, 1000, 450]);

subplot(1, 2, 1); hold on;
scatter(results(:,1), results(:,3), 50, results(:,2), 'filled');
refline(1,0); 
title('Mu Recovery'); xlabel('True Mu'); ylabel('Recovered Mu');
grid on; colorbar;

subplot(1, 2, 2); hold on;
scatter(results(:,2), results(:,4), 50, results(:,1), 'filled');
refline(1,0);
title('Sigma Recovery'); xlabel('True Sigma'); ylabel('Recovered Sigma');
grid on; colorbar;