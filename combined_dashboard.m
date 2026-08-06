%% 1. Define Participant details and Load Data
pat = 'FRTC05'; % Udate with the subject ID you want to inspect
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\Combined_experiment'; 
pat_dir = fullfile(base_dir, pat);

% Load Behavioral Data
analyzed_file = fullfile(pat_dir, [pat, '_combined_reach_data_analyzed.mat']);
load(analyzed_file);
analyzed_data = combined_data; % Table used for SAT, Vigor, RT

% Load Pupil Data
compact_file = fullfile(pat_dir, [pat, '_combined_reach_data_compact.mat']);
load(compact_file);
compact_data = Compact_all; % Table used for Pupil Traces

exclude_blocks = [];

%% 2. Define the 4 conditions and aesthetics
% Target values for [Sham+NoRew, Sham+Rew, tVNS+NoRew, tVNS+Rew]
target_stim = [1, 1, 2, 2];
target_rew  = [3, 4, 3, 4];
cond_labels = {'Sham+NoRew', 'Sham+Rew', 'tVNS+NoRew', 'tVNS+Rew'};
x_positions = 1:4; 

% 4-color palette
colors = { ...
    [0.1 0.1 0.1],  ... % 1. Gray (Sham + No Reward)
    [0.2 0.6 0.86], ... % 2. Blue (Sham + Reward)
    [0 0.7 0],      ... % 3. Green (tVNS + No Reward)
    [0.9 0.3 0.24]  ... % 4. Red (tVNS + Reward)
};

%% 3. Prepare the Dashboard Figure
figure('Name', sprintf('Participant Dashboard - Subject: %s', pat), ...
       'Position', [50 50 1400 900], 'Color', 'w');

%% =========================================================================
%% PANEL 1 (Top-Left): SAT Curves 
%% =========================================================================
subplot(2, 2, 1);
hold on; 
box on; grid on;

% Filter for standard hit trials
beh_data = analyzed_data(analyzed_data.catch_trial == 1 & ~ismember(analyzed_data.block_num, exclude_blocks), :);

% Fit models for the 4 datasets independently
models = cell(1, 4);
for i = 1:4
    idx = (beh_data.stim_type == target_stim(i) & beh_data.reward_type == target_rew(i));
    rt_sec  = beh_data.eff_pt(idx) / 1000;
    hits = beh_data.success(idx);
    models{i} = fit_model(rt_sec, hits);
    
    % Plot the curve
    plot(models{i}.p_hit_plot, 'Color', colors{i}, 'LineWidth', 2.5, 'DisplayName', cond_labels{i});
end

ylim([0 1]);
xlim([0 400]); 
title('Speed-Accuracy Tradeoff (SAT)', 'FontSize', 14);
xlabel('Time (ms)', 'FontSize', 12, 'FontWeight', 'bold');
ylabel('Probability of Hit', 'FontSize', 12, 'FontWeight', 'bold');
legend('Location', 'best', 'FontSize', 10);
set(gca, 'FontSize', 11);
hold off;

%% =========================================================================
%% PANEL 2 (Top-Right): Peak Tangential Velocity (Vigor)
%% =========================================================================
subplot(2, 2, 2);
hold on; 
box on; grid on;

for i = 1:length(x_positions)
    is_current_condition = (analyzed_data.stim_type == target_stim(i)) & ...
                           (analyzed_data.reward_type == target_rew(i)) & ...
                           ~ismember(analyzed_data.block_num, exclude_blocks) & ...
                           (analyzed_data.catch_trial == 1);
    
    current_vel_data = analyzed_data.peak_tanVel(is_current_condition);
    
    % Scatter plot with jitter
    jitter = (rand(size(current_vel_data)) * 0.3) - 0.15;
    scatter(x_positions(i) + jitter, current_vel_data, ...
            30, colors{i}, 'filled', 'MarkerFaceAlpha', 0.3, 'MarkerEdgeColor', 'none');
            
    % Mean Crossbars
    y_mean = mean(current_vel_data, 'omitnan');
    x_bar_start = x_positions(i) - 0.25; 
    x_bar_end   = x_positions(i) + 0.25; 
    plot([x_bar_start, x_bar_end], [y_mean, y_mean], 'Color', colors{i}, 'LineWidth', 4);
    plot([x_bar_start, x_bar_end], [y_mean, y_mean], 'Color', 'k', 'LineWidth', 1);
end

xticks(x_positions); 
xticklabels(cond_labels); 
xtickangle(20);
xlim([0.5 4.5]);
title('Peak Tangential Velocity', 'FontSize', 14);
ylabel('Vigor (peak\_tanVel)', 'FontSize', 12, 'FontWeight', 'bold');
set(gca, 'FontSize', 11);
hold off;

%% =========================================================================
%% PANEL 3 (Bottom-Left): Normalized Pupil Size Traces
%% =========================================================================
subplot(2, 2, 3);
hold on; 
box on; grid on;

for i = 1:4
    matchIdx = compact_data.stim_type == target_stim(i) & ...
               compact_data.reward_type == target_rew(i) & ...
               compact_data.pupil_nan_percent < 0.3;

    cellArrays = compact_data.pupil_size_normalized(matchIdx);
    nonEmptyCells = cellfun(@(x) x', cellArrays(~cellfun(@isempty, cellArrays) & ...
                               ~cellfun(@(x) all(isnan(x)), cellArrays)), ...
                               'UniformOutput', false);
                               
    if ~isempty(nonEmptyCells)
        numericData = cell2mat(nonEmptyCells);
        trialMeans = mean(numericData, 2);
        outlierIdx = isoutlier(trialMeans); 
        
        numericData = numericData(~outlierIdx, :);
        averageResult = mean(numericData, 1);
        
        plot_len = min(7000, length(averageResult));
        plot(1:plot_len, averageResult(1:plot_len), 'Color', colors{i}, 'LineWidth', 2.5);
    end
end

title('Normalized Pupil Size', 'FontSize', 14);
xlabel('Time (samples)', 'FontSize', 12, 'FontWeight', 'bold');
ylabel('Pupil Size (Z-score/Ratio)', 'FontSize', 12, 'FontWeight', 'bold');
set(gca, 'FontSize', 11);
hold off;

%% =========================================================================
%% PANEL 4 (Bottom-Right): Reaction Time (RT)
%% =========================================================================
subplot(2, 2, 4);
hold on; 
box on; grid on;

for i = 1:length(x_positions)
    is_current_condition = (analyzed_data.stim_type == target_stim(i)) & ...
                           (analyzed_data.reward_type == target_rew(i)) & ...
                           ~ismember(analyzed_data.block_num, exclude_blocks) & ...
                           (analyzed_data.catch_trial == 1); 
    
    current_rt_data = analyzed_data.rt(is_current_condition);
    
    % Scatter plot with jitter
    jitter = (rand(size(current_rt_data)) * 0.3) - 0.15; 
    scatter(x_positions(i) + jitter, current_rt_data, ...
            30, colors{i}, 'filled', 'MarkerFaceAlpha', 0.3, 'MarkerEdgeColor', 'none'); 
            
    % Mean Crossbars
    y_mean = mean(current_rt_data, 'omitnan');
    x_bar_start = x_positions(i) - 0.25; 
    x_bar_end   = x_positions(i) + 0.25; 
    plot([x_bar_start, x_bar_end], [y_mean, y_mean], 'Color', colors{i}, 'LineWidth', 4); 
    plot([x_bar_start, x_bar_end], [y_mean, y_mean], 'Color', 'k', 'LineWidth', 1); 
end

xticks(x_positions); 
xticklabels(cond_labels); 
xtickangle(20);
xlim([0.5 4.5]);
title('Reaction Time', 'FontSize', 14); 
ylabel('Reaction Time (rt)', 'FontSize', 12, 'FontWeight', 'bold');
set(gca, 'FontSize', 11);
hold off;

sgtitle(sprintf('Participant Overview Dashboard - %s', pat), 'FontSize', 18, 'FontWeight', 'bold');