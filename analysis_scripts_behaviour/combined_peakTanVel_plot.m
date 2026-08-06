%% Define Participant details
pat = 'FRTC03'; % Update with your new participant ID
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\Combined_experiment'; 
pat_dir = fullfile(base_dir, pat);
data_file = fullfile(pat_dir, [pat, '_combined_reach_data_analyzed.mat']);
load(data_file);

exclude_blocks = [];

% Map the loaded table to a generic variable 
data = combined_data; 

%% Define the 4 conditions
% Target values for [Sham+NoRew, Sham+Rew, tVNS+NoRew, tVNS+Rew]
target_stim = [1, 1, 2, 2];
target_rew  = [3, 4, 3, 4];
x_labels = {'Sham+NoRew', 'Sham+Rew', 'tVNS+NoRew', 'tVNS+Rew'};
x_positions = 1:4; 

% 4-color palette to match the previous plots
colors = { ...
    [0.1 0.1 0.1],  ... % 1. Gray (Sham + No Reward)
    [0.2 0.6 0.86], ... % 2. Blue (Sham + Reward)
    [0 0.7 0],      ... % 3. Green (tVNS + No Reward)
    [0.9 0.3 0.24]  ... % 4. Red (tVNS + Reward)
};

%% Prepare the figure (Wider to accommodate 2 subplots)
figure('Name', strcat('Movement Vigor - Subject: ', pat), 'Position', [100 200 1200 500]);

% Arrays to store stats for the second plot
mean_velocities = zeros(1, 4);
sem_velocities = zeros(1, 4);

%% ---------------------------------------------------------
%% SUBPLOT 1: All Trials (Scatter) + Mean Crossbars
%% ---------------------------------------------------------
subplot(1, 2, 1);
hold on; 
box on;
grid on;

% Loop through each condition to plot scatter and calculate means
for i = 1:length(x_positions)
    
    is_current_condition = (data.stim_type == target_stim(i)) & ...
                           (data.reward_type == target_rew(i)) & ...
                           ~ismember(data.block_num, exclude_blocks) & ...
                           (data.catch_trial == 1); 
    
    current_vel_data = data.peak_tanVel(is_current_condition);
    
    % Calculate the mean and SEM for this condition
    mean_velocities(i) = mean(current_vel_data, 'omitnan');
    sem_velocities(i)  = std(current_vel_data, 'omitnan') / sqrt(sum(~isnan(current_vel_data)));
    
    % Scatter plot with jitter
    jitter = (rand(size(current_vel_data)) * 0.3) - 0.15;
    scatter(x_positions(i) + jitter, current_vel_data, ...
            30, colors{i}, 'filled', ... 
            'MarkerFaceAlpha', 0.3, 'MarkerEdgeColor', 'none'); 
end

% Plot the Mean Crossbars over the scatter
for i = 1:length(x_positions)
    x_bar_start = x_positions(i) - 0.25; 
    x_bar_end   = x_positions(i) + 0.25; 
    y_mean      = mean_velocities(i);        
    
    plot([x_bar_start, x_bar_end], [y_mean, y_mean], 'Color', colors{i}, 'LineWidth', 4); 
    plot([x_bar_start, x_bar_end], [y_mean, y_mean], 'Color', 'k', 'LineWidth', 1);
end

% Subplot 1 Aesthetics
xticks(x_positions); 
xticklabels(x_labels); 
xtickangle(20);
xlabel('Condition', 'FontSize', 12, 'FontWeight', 'bold');
ylabel('Peak Tangential Velocity (Vigor)', 'FontSize', 12, 'FontWeight', 'bold');
title('Individual Trials and Means', 'FontSize', 14);
xlim([0.5 4.5]);
set(gca, 'FontSize', 11);
hold off;

%% ---------------------------------------------------------
%% SUBPLOT 2: Means Only (with SEM Error Bars)
%% ---------------------------------------------------------
subplot(1, 2, 2);
hold on;
box on;
grid on;


% Plot Error bars and Mean dots
for i = 1:length(x_positions)
    % Plot SEM error bar first so it sits behind the dot
    errorbar(x_positions(i), mean_velocities(i), sem_velocities(i), ...
             'k', 'LineWidth', 1.5, 'CapSize', 8);
         
    % Plot large mean dot
    plot(x_positions(i), mean_velocities(i), 'o', ...
         'MarkerSize', 12, 'MarkerFaceColor', colors{i}, 'MarkerEdgeColor', 'k', 'LineWidth', 1.2);
end

% Subplot 2 Aesthetics
xticks(x_positions); 
xticklabels(x_labels); 
xtickangle(20);
xlabel('Condition', 'FontSize', 12, 'FontWeight', 'bold');
ylabel('Peak Tangential Velocity (Vigor)', 'FontSize', 12, 'FontWeight', 'bold');
title('Condition Means (\pm SEM)', 'FontSize', 14);
xlim([0.5 4.5]);

% Dynamically tighten the Y-axis based on the min/max means to exaggerate differences
y_min = min(mean_velocities - sem_velocities) * 0.95;
y_max = max(mean_velocities + sem_velocities) * 1.05;
ylim([y_min, y_max]);

set(gca, 'FontSize', 11);
hold off;

% Add a super-title to the figure
sgtitle(sprintf('Movement Vigor Analysis - Subject: %s', pat), 'FontSize', 16, 'FontWeight', 'bold');