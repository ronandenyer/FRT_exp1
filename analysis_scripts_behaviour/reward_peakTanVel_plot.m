%% Define Participant details
pat = 'FRTP03';
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\Reward_experiment'; 
pat_dir = strcat(base_dir, '\', pat);

reward_file = strcat(pat_dir, '\', pat, '_reward_reach_data_analyzed.mat');
load(reward_file);
exclude_blocks = [];

%%
% --- MATLAB Code to Generate the Plot ---

% Assuming your table is called 'reward_data'

% 1. Define the unique conditions (reward_type 3 and 4)
reward_conditions = unique(reward_data.reward_type);

% 2. Prepare the figure
figure;
hold on; % Allow multiple plot elements on the same axes
box on;
grid on;

% 3. Initialize arrays to store mean values
mean_velocities = zeros(size(reward_conditions));

% Define x-axis labels for clarity
x_labels = {'No Reward (3)', 'Reward (4)'};
x_positions = 1:length(reward_conditions); % x-axis positions for plotting

% 4. Loop through each condition (reward_type)
for i = 1:length(reward_conditions)
    current_condition = reward_conditions(i);
    
    % Find the rows corresponding to the current condition
    is_current_condition = (reward_data.reward_type == current_condition);
    current_vel_data = reward_data.peak_tanVel(is_current_condition);
    
    % Calculate the mean velocity for this condition
    mean_velocities(i) = mean(current_vel_data);
    
    % --- Plotting Individual Trial Data (Scatter/Jitter) ---
    
    % To prevent overlap, we add a random offset (jitter) to the x-position.
    % The '0.15' controls the width of the spread.
    jitter = (rand(size(current_vel_data)) * 0.3) - 0.15;
    
    % Scatter plot the individual peak_tanVel values
    scatter(x_positions(i) + jitter, current_vel_data, ...
            30, 'k', 'filled', ... % Size 30, black ('k'), filled circle
            'MarkerFaceAlpha', 0.4); % Transparency for visibility

end

% 5. Plot the Mean Crossbars
for i = 1:length(reward_conditions)
    % Define the horizontal line segment for the crossbar
    x_bar_start = x_positions(i) - 0.2; % Start 0.2 units left of center
    x_bar_end = x_positions(i) + 0.2;   % End 0.2 units right of center
    y_mean = mean_velocities(i);        % The calculated mean
    
    % Plot the crossbar
    plot([x_bar_start, x_bar_end], [y_mean, y_mean], ...
         'r', 'LineWidth', 3); % Red line, 3 pixels thick
end

% 6. Customize the plot appearance
xticks(x_positions); % Set x-ticks to the positions [1, 2]
xticklabels(x_labels); % Label the x-ticks descriptively

% Add labels and title
xlabel('Reward Type');
ylabel('Peak Tangential Velocity');
title('Individual Trials and Mean Peak Velocity by Reward Condition');

% Add a simple legend (optional, as the plot elements are clear)
% plot(NaN, NaN, 'ko', 'MarkerFaceColor', [0.4 0.4 0.4], 'DisplayName', 'Individual Trial'); % Dummy scatter for legend
% plot(NaN, NaN, 'r-', 'LineWidth', 3, 'DisplayName', 'Condition Mean'); % Dummy line for legend
% legend('Location', 'best');

hold off;