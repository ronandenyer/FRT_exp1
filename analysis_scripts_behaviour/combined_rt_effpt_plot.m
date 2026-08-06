%% Define Participant details
pat = 'FRTC05'; % Update with your participant ID
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

%% Prepare the figure (2x2 Grid)
figure('Name', strcat('Timing Analysis - Subject: ', pat), 'Position', [100 100 1200 900]);

% Define the metrics we want to plot
metrics = {'rt', 'eff_pt'};
y_labels = {'Reaction Time (rt)', 'Effective Prep Time (eff\_pt)'};
titles_scatter = {'Individual Trials & Means: RT', 'Individual Trials & Means: Eff PT'};
titles_means = {'Condition Means (\pm SEM): RT', 'Condition Means (\pm SEM): Eff PT'};

%% Loop through both metrics to generate the rows
for m = 1:length(metrics)
    metric_name = metrics{m};
    
    % Arrays to store stats for the second plot in this row
    mean_vals = zeros(1, 4);
    sem_vals = zeros(1, 4);
    
    %% ---------------------------------------------------------
    %% LEFT COLUMN: All Trials (Scatter) + Mean Crossbars
    %% Subplots 1 (RT) and 3 (Eff PT)
    %% ---------------------------------------------------------
    subplot(2, 2, 2*m - 1);
    hold on; 
    box on;
    grid on;

    % Loop through each condition to plot scatter and calculate means
    for i = 1:length(x_positions)
        
        is_current_condition = (data.stim_type == target_stim(i)) & ...
                               (data.reward_type == target_rew(i)) & ...
                               ~ismember(data.block_num, exclude_blocks) & ...
                               (data.catch_trial == 1); 
        
        % Extract the data for the current metric dynamically
        current_metric_data = data.(metric_name)(is_current_condition);
        
        % Calculate the mean and SEM for this condition
        mean_vals(i) = mean(current_metric_data, 'omitnan');
        sem_vals(i)  = std(current_metric_data, 'omitnan') / sqrt(sum(~isnan(current_metric_data)));
        
        % Scatter plot with jitter
        jitter = (rand(size(current_metric_data)) * 0.3) - 0.15;
        scatter(x_positions(i) + jitter, current_metric_data, ...
                30, colors{i}, 'filled', ... 
                'MarkerFaceAlpha', 0.3, 'MarkerEdgeColor', 'none'); 
    end

    % Plot the Mean Crossbars over the scatter
    for i = 1:length(x_positions)
        x_bar_start = x_positions(i) - 0.25; 
        x_bar_end   = x_positions(i) + 0.25; 
        y_mean      = mean_vals(i);        
        
        plot([x_bar_start, x_bar_end], [y_mean, y_mean], 'Color', colors{i}, 'LineWidth', 4); 
        plot([x_bar_start, x_bar_end], [y_mean, y_mean], 'Color', 'k', 'LineWidth', 1);
    end

    % Subplot Aesthetics
    xticks(x_positions); 
    xticklabels(x_labels); 
    xtickangle(20);
    xlabel('Condition', 'FontSize', 12, 'FontWeight', 'bold');
    ylabel(y_labels{m}, 'FontSize', 12, 'FontWeight', 'bold');
    title(titles_scatter{m}, 'FontSize', 14);
    xlim([0.5 4.5]);
    set(gca, 'FontSize', 11);
    hold off;

    %% ---------------------------------------------------------
    %% RIGHT COLUMN: Means Only (with SEM Error Bars)
    %% Subplots 2 (RT) and 4 (Eff PT)
    %% ---------------------------------------------------------
    subplot(2, 2, 2*m);
    hold on;
    box on;
    grid on;

    % Plot Error bars and Mean dots
    for i = 1:length(x_positions)
        % Plot SEM error bar first so it sits behind the dot
        errorbar(x_positions(i), mean_vals(i), sem_vals(i), ...
                 'k', 'LineWidth', 1.5, 'CapSize', 8);
             
        % Plot large mean dot
        plot(x_positions(i), mean_vals(i), 'o', ...
             'MarkerSize', 12, 'MarkerFaceColor', colors{i}, 'MarkerEdgeColor', 'k', 'LineWidth', 1.2);
    end

    % Subplot Aesthetics
    xticks(x_positions); 
    xticklabels(x_labels); 
    xtickangle(20);
    xlabel('Condition', 'FontSize', 12, 'FontWeight', 'bold');
    ylabel(y_labels{m}, 'FontSize', 12, 'FontWeight', 'bold');
    title(titles_means{m}, 'FontSize', 14);
    xlim([0.5 4.5]);

    % Dynamically tighten the Y-axis based on the min/max means to exaggerate differences
    y_min = min(mean_vals - sem_vals) * 0.95;
    y_max = max(mean_vals + sem_vals) * 1.05;
    
    % Safeguard in case of flat data (prevents ylim error)
    if y_max > y_min 
        ylim([y_min, y_max]);
    end

    set(gca, 'FontSize', 11);
    hold off;
end

% Add a super-title to the figure
sgtitle(sprintf('Timing Analysis (RT & Prep Time) - Subject: %s', pat), 'FontSize', 16, 'FontWeight', 'bold');