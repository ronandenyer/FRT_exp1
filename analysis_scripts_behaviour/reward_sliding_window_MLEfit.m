%% Define Participant details
pat = 'FRTP30';
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\Reward_experiment'; 
pat_dir = strcat(base_dir, '\', pat);
reward_file = strcat(pat_dir, '\', pat, '_reward_reach_data_analyzed.mat');
load(reward_file);

exclude_blocks = [];
kinarm_delay = 0.02; % KINARM system delay in seconds

%% 1. Data Cleaning and Filtering
% Filter for non-catch trials and excluded blocks
valid_idx = (reward_data.catch_trial == 1) & (~ismember(reward_data.block_num, exclude_blocks));
clean_data = reward_data(valid_idx, :);

% Define adjusted RT and variables
rt_adjusted = (clean_data.eff_pt / 1000) - kinarm_delay; 
hit = clean_data.success;
reward_type = clean_data.reward_type;

%% 2. Fit model
model = reward_fit_unified_model_logistic(rt_adjusted, hit, reward_type);

%% 3. Create Overlaid Plot
figure('Name', ['Overlaid Analysis: ' pat], 'Color', 'w');
hold on;

% --- STEP A: Plot the Sliding Window Accuracy first ---
% This acts as your "Background" or "Raw Data" visualization
% Note: Ensure your function doesn't call 'clf' or 'figure' internally.
reward_plot_sliding_window_accuracy(rt_adjusted, hit, reward_type);

% --- STEP B: Overlay the Psychometric Fits ---
% We use thick lines to ensure they are visible over the sliding window
time_axis = model.t_plot; 
p_hit_noreward = model.p_hit_plot_sham;
p_hit_reward = model.p_hit_plot_stim;

% Plot the No-Reward Fit (Blue)
plot(time_axis, p_hit_noreward, 'Color', [0 0.4470 0.7410], 'LineWidth', 4, ...
     'DisplayName', 'Model: No-Reward');

% Plot the Reward Fit (Red)
plot(time_axis, p_hit_reward, 'Color', [0.8500 0.3250 0.0980], 'LineStyle', '--', 'LineWidth', 4, ...
     'DisplayName', 'Model: Reward');

% --- STEP C: Final Formatting ---
title(['Speed-Accuracy Tradeoff: ' pat], 'FontSize', 16);
xlabel('Preparation/Reaction Time (s)', 'FontSize', 14);
ylabel('Accuracy (P_{Hit})', 'FontSize', 14);

% Sync the limits
ylim([0 1.05]); % Slightly above 1 for legend room
xlim([0 max(rt_adjusted) + 0.1]);

grid on;
box on;
legend('Location', 'southeast', 'FontSize', 10);
hold off;