%% Define Participant details
pat = 'FRTP15';
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\tvns_experiment'; 
pat_dir = strcat(base_dir, '\', pat);

tvns_file = strcat(pat_dir, '\', pat, '_tvns_reach_data_analyzed.mat');
load(tvns_file);
tvns_data = tvns_data_light;
exclude_blocks = [];
%%
% --- MATLAB Code for tVNS Moving Average and SEM Shading ---

% Assuming tvns_data is loaded and has columns 'peak_tanVel', 'eff_pt', and 'stim_type'

% 1. Define conditions and window size (N points)
STIM_SHAM = 1;
STIM_TVNS = 2;

% *** ADJUST THIS VALUE: Number of data points (rows) to average over.
WINDOW_SIZE_POINTS = 100; 

% Define colors (Using the same Speed-Accuracy style colors as the reward plot)
COLOR_SHAM = [0 0.4470 0.7410]; % Blue for Sham
COLOR_TVNS = [0.8500 0.3250 0.0980];    % Red/Orange for tVNS

% 2. Loop through conditions and process data
all_data_to_plot = cell(2, 1);

for condition_idx = 1:2
    
    if condition_idx == 1
        current_stim_type = STIM_SHAM;
        current_color = COLOR_SHAM;
        current_label = 'Sham (1)';
    else
        current_stim_type = STIM_TVNS;
        current_color = COLOR_TVNS;
        current_label = 'tVNS (2)';
    end

    % Filter the data for the current stimulation type
    current_data = tvns_data(tvns_data.stim_type == current_stim_type, :);

    % --- HANDLING NaNs and Sorting ---
    % Remove rows where *either* peak_tanVel or eff_pt is NaN
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

    % Calculate the N-point moving Standard Error of the Mean (SEM = SD / sqrt(N))
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
    upper_bound = data.mean + data.sem;
    lower_bound = data.mean - data.sem;
    
    % Define the vertices for the polygon (time forward, then time backward)
    fill_x = [data.eff_pt; flipud(data.eff_pt)];
    fill_y = [upper_bound; flipud(lower_bound)];
    
    fill(fill_x, fill_y, data.color, ...
         'FaceAlpha', 0.2, ...        % Transparent shading (20%)
         'EdgeColor', 'none');        % No border
    
    % --- 3b. Plot the Smoothed Mean Line ---
    p(i) = plot(data.eff_pt, data.mean, ...
         'Color', data.color, ...
         'LineWidth', 2, ...
         'DisplayName', data.label);
end

% 4. Customization
xlabel('Effective Point (eff\_pt, ms)');
ylabel('Smoothed Peak Tangential Velocity');
title(sprintf('N=%d Point Moving Average with SEM by Stimulation Condition', WINDOW_SIZE_POINTS));

% Add a legend
legend(p, 'Location', 'best');

hold off;