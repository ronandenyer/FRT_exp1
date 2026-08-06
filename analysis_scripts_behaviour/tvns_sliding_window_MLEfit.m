%% Define Participant details
pat = 'FRTP31';
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\tVNS_experiment'; 
pat_dir = strcat(base_dir, '\', pat);
tvns_file = strcat(pat_dir, '\', pat, '_tvns_reach_data_analyzed.mat');
load(tvns_file);
tvns_data = tvns_data_light;
exclude_blocks = [];
kinarm_delay = 0.0; % KINARM system delay in seconds

%% 1. Data Cleaning and Filtering
% Filter for non-catch trials and excluded blocks
valid_idx = (tvns_data.catch_trial == 1) & (~ismember(tvns_data.block_num, exclude_blocks));
clean_data = tvns_data(valid_idx, :);

% Define adjusted RT and variables
rt_adjusted = (clean_data.eff_pt / 1000) - kinarm_delay; 
hit = clean_data.success;
stim_type = clean_data.stim_type;

%% 2. Fit model
%model = tvns_fit_unified_model_logistic(rt_adjusted, hit, stim_type);
model_sham = fit_model(rt_adjusted(stim_type == 1), hit(stim_type == 1));
model_stim = fit_model(rt_adjusted(stim_type == 2), hit(stim_type == 2));

%% 3. Create Overlaid Plot
%figure('Name', ['Overlaid Analysis: ' pat], 'Color', 'w');
hold on;

% --- STEP A: Plot the Sliding Window Accuracy first ---
% This acts as your "Background" or "Raw Data" visualization
% Note: Ensure your function doesn't call 'clf' or 'figure' internally.
tvns_plot_sliding_window_accuracy(rt_adjusted, hit, stim_type);

% --- STEP B: Overlay the Psychometric Fits ---
% We use thick lines to ensure they are visible over the sliding window
time_axis = model.t_plot; 
p_hit_sham = model_sham.p_hit_plot;
p_hit_stim = model_stim.p_hit_plot;

% Plot the Sham Fit (Blue)
plot(time_axis, p_hit_sham, 'Color', [0.2 0.2 0.2], 'LineWidth', 4, ...
     'DisplayName', 'Model: No-Reward');

% Plot the tVNS Fit (Red)
plot(time_axis, p_hit_stim, 'Color', [0.0 0.30 0.60], 'LineStyle', '--', 'LineWidth', 4, ...
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