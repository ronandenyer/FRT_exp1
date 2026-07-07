%% Define Participant details
pat = 'FRTP22';
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\tvns_experiment'; 
pat_dir = strcat(base_dir, '\', pat);

tvns_file = strcat(pat_dir, '\', pat, '_tvns_reach_data_analyzed.mat');
load(tvns_file);
tvns_data = tvns_data_light;
exclude_blocks = [];

%%
% --- MATLAB Code to Generate the Plot ---

% Assuming your table is called 'tvns_data'
% and stim_type contains only 1 (Sham) and 2 (tVNS)

% 1. Define the unique conditions and their labels
stim_conditions = [1, 2]; % Must be in the order you want them plotted (1 then 2)

% Define descriptive x-axis labels
x_labels = {'Sham', 'tVNS'};
x_positions = 1:length(stim_conditions); % x-axis positions will be 1 and 2

% 2. Prepare the figure
figure;
hold on; % Allow multiple plot elements on the same axes
box on;
grid on;

% 3. Initialize arrays to store mean values
mean_velocities = zeros(size(stim_conditions));

% 4. Loop through each condition (stim_type = 1 and 2)
for i = 1:length(stim_conditions)
    current_condition = stim_conditions(i);
    
    % Find the rows corresponding to the current condition (1 or 2)
    is_current_condition = (tvns_data.stim_type == current_condition);
    current_vel_data = tvns_data.peak_tanVel(is_current_condition);
    
    % Calculate the mean velocity for this condition
    mean_velocities(i) = mean(current_vel_data);
    
    % --- Plotting Individual Trial Data (Scatter/Jitter) ---
    
    % Add a random offset (jitter) to the x-position for visibility
    % Jitter range is controlled by the multiplier (0.3 total width)
    jitter = (rand(size(current_vel_data)) * 0.3) - 0.15;
    
    % Scatter plot the individual peak_tanVel values
    scatter(x_positions(i) + jitter, current_vel_data, ...
            30, 'k', 'filled', ...    % Size 30, black ('k'), filled circle
            'MarkerFaceAlpha', 0.4); % 40% transparency
end

% 5. Plot the Mean Crossbars
for i = 1:length(stim_conditions)
    % Define the horizontal line segment for the crossbar
    x_bar_start = x_positions(i) - 0.2; % Start 0.2 units left of center
    x_bar_end = x_positions(i) + 0.2;   % End 0.2 units right of center
    y_mean = mean_velocities(i);        % The calculated mean
    
    % Plot the crossbar
    plot([x_bar_start, x_bar_end], [y_mean, y_mean], ...
         'r', 'LineWidth', 3); % Red line, 3 pixels thick
end

% 6. Customize the plot appearance
xticks(x_positions);    % Set x-ticks to the positions [1, 2]
xticklabels(x_labels);  % Label the x-ticks with 'Sham' and 'tVNS'

% Add labels and title
xlabel('Stimulation Condition');
ylabel('Peak Tangential Velocity');
title('Peak Tangential Velocity: Sham vs. tVNS Trials');

hold off;