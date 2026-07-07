%% Define Participant details
pat = 'RONAN';
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\Reward_experiment'; 
pat_dir = strcat(base_dir, '\', pat);

reward_file = strcat(pat_dir, '\', pat, '_reward_reach_data_analyzed.mat');
load(reward_file);
exclude_blocks = [0];
kinarm_delay = 0.02; %KINARM system delay in secondsblocks = [];
reward_data = reward_data(~ismember(reward_data.block_num, exclude_blocks),:);
%%
% --- FINAL MATLAB Code with Moving Average and SEM Shading ---

% Assuming reward_data is loaded and has columns 'peak_tanVel', 'eff_pt', and 'reward_type'

% 1. Define conditions and window size (N points)
REWARD_NO = 3;
REWARD_YES = 4;

% *** ADJUST THIS VALUE: Number of data points (rows) to average over.
WINDOW_SIZE_POINTS = 100; 

% Define colors for the Speed-Accuracy Curve style plot (Blue and Red)
COLOR_NO_REWARD = [0 0.4470 0.7410]; % A strong Blue
COLOR_REWARD = [0.8500 0.3250 0.0980];    % A strong Red/Orange

% 2. Loop through conditions and process data
all_data_to_plot = cell(2, 1);

for condition_idx = 1:2
    
    if condition_idx == 1
        current_reward_type = REWARD_NO;
        current_color = COLOR_NO_REWARD;
        current_label = 'No Reward (3)';
    else
        current_reward_type = REWARD_YES;
        current_color = COLOR_REWARD;
        current_label = 'Reward (4)';
    end

    % Filter the data for the current reward type
    current_data = reward_data(reward_data.reward_type == current_reward_type, :);

    % --- HANDLING NaNs and Sorting ---
    nan_mask = isnan(current_data.peak_tanVel) | isnan(current_data.eff_pt);
    current_data(nan_mask, :) = [];

    % IMPORTANT: Sort data by the time measure (eff_pt)
    current_data = sortrows(current_data, 'eff_pt');

    % Extract the vectors needed
    vel_vector = current_data.peak_tanVel;
    
    % --- MOVING MEAN and MOVING SEM CALCULATION ---
    
    % Calculate the N-point moving average
    current_data.smoothed_vel = movmean(vel_vector(:), WINDOW_SIZE_POINTS); 
    
    % Calculate the N-point moving Standard Deviation (SD)
    moving_sd = movstd(vel_vector(:), WINDOW_SIZE_POINTS);

    % Calculate the N-point moving Standard Error of the Mean (SEM)
    % SEM = SD / sqrt(N)
    % Note: While movmean and movstd calculate over the window, the 'N' here 
    % is the fixed window size (WINDOW_SIZE_POINTS), as the effective N is constant.
    current_data.smoothed_sem = moving_sd ./ sqrt(WINDOW_SIZE_POINTS); 
    
    % Store processed data for plotting
    all_data_to_plot{condition_idx} = struct('eff_pt', current_data.eff_pt, ...
                                             'mean', current_data.smoothed_vel, ...
                                             'sem', current_data.smoothed_sem, ...
                                             'color', current_color, ...
                                             'label', current_label);
end


% 3. Create the plot
figure;
hold on;
box on;
grid on;

% Plot the shaded error regions and lines
p = gobjects(2, 1);
for i = 1:2
    data = all_data_to_plot{i};
    
    % --- 3a. Plot the Shaded SEM Area using FILL ---
    % Define the upper and lower bounds of the shaded region
    upper_bound = data.mean + data.sem;
    lower_bound = data.mean - data.sem;
    
    % Define the vertices for the polygon (time forward, then time backward)
    fill_x = [data.eff_pt; flipud(data.eff_pt)];
    fill_y = [upper_bound; flipud(lower_bound)];
    
    fill(fill_x, fill_y, data.color, ...
         'FaceAlpha', 0.2, ...        % Set high transparency (20%)
         'EdgeColor', 'none');        % No border around the shaded area
    
    % --- 3b. Plot the Smoothed Mean Line ---
    p(i) = plot(data.eff_pt, data.mean, ...
         'Color', data.color, ...
         'LineWidth', 2, ...
         'DisplayName', data.label);
end

% 4. Customization
xlabel('Effective Point (eff\_pt, ms)');
ylabel('Smoothed Peak Tangential Velocity');
title(sprintf('N=%d Point Moving Average with SEM by Reward Condition', WINDOW_SIZE_POINTS));

% Add a legend
legend(p, 'Location', 'best');

hold off;