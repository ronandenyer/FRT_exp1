%% Define Participant details
pat = 'FRTP14';
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\Reward_experiment'; 
pat_dir = strcat(base_dir, '\', pat);

reward_file = strcat(pat_dir, '\', pat, '_reward_reach_data_analyzed.mat');
load(reward_file);
exclude_blocks = [];
kinarm_delay = 0.02; %KINARM system delay in seconds
%% Define RT_array and Hit arrays (reward and non-reward)
reward_data = reward_data(reward_data.catch_trial == 1,:); % remove catch trials

rt = reward_data.eff_pt(~ismember(reward_data.block_num, exclude_blocks))/1000-kinarm_delay; % define rt array for no reward trials, divide by 1000 as fit_model expects data in seconds

hit = reward_data.success(~ismember(reward_data.block_num, exclude_blocks)); % define hit array for no reward trials (1 = hit, 0 = miss)
reward_type = reward_data.reward_type(~ismember(reward_data.block_num, exclude_blocks)); % define hit array for no reward trials


%% fit model to reward and no reward data
model = reward_fit_unified_model_logistic(rt, hit, reward_type);
disp(model.paramsOpt);
%% Visualize results

% The time vector for the smooth fitted curves
time_axis = model.t_plot; 

% The predicted probability curves
p_hit_noreward = model.p_hit_plot_noreward;
p_hit_reward = model.p_hit_plot_reward;

% --- NEW: Calculate the average of the two fitted curves ---
% This serves as a visualization of the overall trend across both conditions.
p_hit_average = (p_hit_noreward + p_hit_reward) / 2;


% --- 2. PLACEHOLDER FOR RAW DATA (Replace with your actual binned data) ---
% Raw data is essential to show how well the fit captures the actual measurements.
% Example structure (replace with your binned RT and accuracy data):
% raw_rt_sham = [0.1, 0.2, 0.3, 0.4]; 
% raw_acc_sham = [0.28, 0.60, 0.85, 0.93];
% 
% raw_rt_stim = [0.08, 0.18, 0.28, 0.38]; 
% raw_acc_stim = [0.28, 0.70, 0.90, 0.96];


% --- 3. Create the Figure and Plot the Smooth Fits ---

figure('Name', 'Unified Psychometric Model Fit', 'NumberTitle', 'off'); 
hold on; 

% Plot the SHAM (Control) fitted curve
plot(time_axis, p_hit_noreward, 'b-', 'LineWidth', 3, 'DisplayName', 'No Reward (Fitted Curve)');

% Plot the STIMULATION fitted curve
plot(time_axis, p_hit_reward, 'r--', 'LineWidth', 3, 'DisplayName', 'Reward (Fitted Curve)');

% --- NEW: Plot the Average Fit as a faint gray dotted line ---
plot(time_axis, p_hit_average, 'Color', [0.6 0.6 0.6], 'LineStyle', ':', 'LineWidth', 2, ...
     'DisplayName', 'Average Fit (Overall Trend)');


% --- 4. Plot the Raw Data Points (Uncomment and Replace with Actual Data) ---
% scatter(raw_rt_sham, raw_acc_sham, 50, 'b', 'filled', 'DisplayName', 'Sham (Raw Data)');
% scatter(raw_rt_stim, raw_acc_stim, 50, 'r', 'filled', 'DisplayName', 'Stimulation (Raw Data)');

% --- 5. Final Plot Aesthetics ---

% Set axis limits
ylim([0 1]); 
xlim([0 max(time_axis)]);

% Add labels and title
xlabel('Preparation/Reaction Time (s)', 'FontSize', 14);
ylabel('Probability of Correct Response (P_{Hit})', 'FontSize', 14);
title('Effect of Reward on Speed-Accuracy Function', 'FontSize', 16);

% Add a grid and improve box appearance
grid on;
box on;
set(gca, 'FontSize', 12);

% Add a legend
legend('Location', 'southeast');

hold off;