%% AGGREGATION SCRIPT: Calculate Deltas Per Subject (UPDATED with model fit)

% --- 1. Define Participants and File Structure ---
% List of all Participant IDs (SIDs)
participants = {'FRTP01', 'FRTP02', 'FRTP03', 'FRTP04', 'FRTP05', 'FRTP06', ...
                'FRTP07', 'FRTP09', 'FRTP11', 'FRTP12', ...
                'FRTP14', 'FRTP16', 'FRTP17', 'FRTP18', 'FRTP20', 'FRTP21',  ...
                'FRTP22','FRTP23', 'FRTP24', 'FRTP25', 'FRTP26', 'FRTP27', 'FRTP28', ...
                'FRTP29', 'FRTP30', 'FRTP31', 'FRTP32', 'FRTP33'};% <-- REPLACE with your actual list of SIDs!
%% REWARD EXPERIMENT: Correlation of Model Delta Mu vs. Velocity Delta (Reward - NoReward)

% --- 1. Define Participants and File Structure ---
% !!! REPLACE BASE DIRECTORY FOR REWARD EXPERIMENT !!!
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\reward_experiment'; 
kinarm_delay = 0.02;
% Initialize table to store the final delta results
% Delta_Vel (Y-axis): Mean(peak_tanVel_Reward) - Mean(peak_tanVel_NoReward)
% Delta_Mu (X-axis):  mu_Reward - mu_NoReward = +p5
subject_deltas_reward = table('Size', [length(participants), 3], ...
                       'VariableTypes', {'string', 'double', 'double'}, ...
                       'VariableNames', {'SID', 'Delta_Vel', 'Delta_Mu'}); 

% --- 2. Loop Through Participants and Calculate Deltas ---
for p = 1:length(participants)
    pat = participants{p};
    
    % Construct file path and load data
    pat_dir = fullfile(base_dir, pat);
    reward_file = fullfile(pat_dir, [pat, '_reward_reach_data_analyzed.mat']); 
    
    if exist(reward_file, 'file') ~= 2
        warning('File not found for participant %s. Skipping.', pat);
        continue;
    end
    
    load(reward_file);
    
    % --- 2a. Calculate Mean Peak Velocity Delta (Reward - NoReward) ---
    REWARD_NO = 3;
    REWARD_YES = 4;
    
    noreward_data = reward_data(reward_data.reward_type == REWARD_NO & reward_data.catch_trial == 1, :);
    reward_sub_data = reward_data(reward_data.reward_type == REWARD_YES & reward_data.catch_trial == 1, :);
    
    mean_vel_noreward = mean(noreward_data.peak_tanVel, 'omitnan');
    mean_vel_reward = mean(reward_sub_data.peak_tanVel, 'omitnan');
    
    % Delta Velocity: (Reward - NoReward)
    delta_vel = mean_vel_reward- mean_vel_noreward; 

    % --- 2b. Prepare Inputs for Model Fit ---
    RT_combined = (reward_data.eff_pt(:) / 1000) - kinarm_delay; 
    Hit_combined = reward_data.success(:); 
    Reward_Condition_Input = reward_data.reward_type(:); 
    
    % --- 2c. Execute Unified Model Fit ---
    Delta_mu = NaN; 
    try
        model_output = reward_fit_unified_model_logistic(RT_combined, Hit_combined, Reward_Condition_Input);
        
        % The model's p5 is already (Reward - NoReward), so we use it directly.
        Delta_mu = model_output.paramsOpt(5); 
        
    catch ME
        warning('Model fit failed for %s. Error: %s. Setting model delta to NaN.', pat, ME.message);
    end
    
    % --- 2d. Store Results in the Aggregation Table ---
    subject_deltas_reward.SID(p) = pat;
    subject_deltas_reward.Delta_Vel(p) = delta_vel;
    subject_deltas_reward.Delta_Mu(p) = Delta_mu; 
end

% --- 3. Final Filtering and Plotting ---
subject_deltas = subject_deltas_reward; 
nan_mask = isnan(subject_deltas.Delta_Mu) | isnan(subject_deltas.Delta_Vel);
subject_deltas(nan_mask, :) = [];

disp('--- Reward Experiment Delta Results Per Subject (Reward - NoReward) ---');
disp(subject_deltas);

%% PLOTTING SCRIPT: Correlation Analysis (Model Delta Mu vs. Velocity Delta)

% 1. Prepare data (use the final filtered subject_deltas table)
X = subject_deltas.Delta_Mu;
Y = subject_deltas.Delta_Vel;

if length(X) < 2
    warning('Insufficient data points for correlation plot (N < 2). Cannot calculate regression or R-value.');
    return;
end

figure;
hold on;
box on;
grid on;

% Scatter Plot
scatter(X, Y, ...
        80, 'filled', ... 
        'MarkerFaceColor', [0.85 0.325 0.098], ... 
        'MarkerEdgeColor', 'k');

% Regression Line
P = polyfit(X, Y, 1); 
X_fit = linspace(min(X), max(X), 100);
Y_fit = polyval(P, X_fit);
plot(X_fit, Y_fit, 'b-', 'LineWidth', 2); 

% Correlation Statistics
[R, Pval] = corr(X, Y, 'Type', 'Pearson');

% Axes lines at zero 
plot([0 0], ylim, 'k--', 'HandleVisibility', 'off'); 
plot(xlim, [0 0], 'k--', 'HandleVisibility', 'off'); 

% Axes labels and title
xlabel('\Delta Mean Time (\Delta\mu) from Unified Model (Reward - NoReward, s)');
ylabel('\Delta Mean Peak Tangential Velocity (Reward - NoReward, units/s)');
title(sprintf('Reward Exp: Correlation between \\Delta Mean Time and \\Delta Peak Velocity\n(r = %.2f, p = %.3f)', R, Pval));

hold off;