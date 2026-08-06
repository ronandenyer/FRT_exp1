%% AGGREGATION SCRIPT: Mean Peak Velocity per Subject
participants = {'FRTP01', 'FRTP02', 'FRTP03', 'FRTP04', 'FRTP05', 'FRTP06', 'FRTP07', ...
    'FRTP09', 'FRTP11', 'FRTP12', 'FRTP14', 'FRTP16', 'FRTP17', ...
    'FRTP18', 'FRTP20', 'FRTP21', 'FRTP22', 'FRTP23', 'FRTP24', 'FRTP25', 'FRTP26', ...
    'FRTP27', 'FRTP28', 'FRTP29', 'FRTP30'}; 
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\reward_experiment'; 

% Initialize table to store one row per participant per condition
subject_means = table();

fprintf('Calculating subject-level means...\n');

for p = 1:length(participants)
    pat = participants{p};
    pat_file = fullfile(base_dir, pat, [pat, '_reward_reach_data_analyzed.mat']); 
    if ~exist(pat_file, 'file'); continue; end
    load(pat_file);
    
    REWARD_NO = 3;
    REWARD_YES = 4;
   
    % Define outlier filter (1.5 * IQR)
    filter_outliers = @(data) data(data >= (quantile(data, 0.25) - 1.5*iqr(data)) & ...
                                   data <= (quantile(data, 0.75) + 1.5*iqr(data)));
    
    % 1. Process No-Reward Mean
    v_no_raw = reward_data.peak_tanVel(reward_data.reward_type == REWARD_NO);
    v_no_clean = filter_outliers(v_no_raw(~isnan(v_no_raw)));
    mean_no = mean(v_no_clean);
    mean_no_pct = (mean_no/mean_no)*100;
    
    % 2. Process Reward Mean
    v_rew_raw = reward_data.peak_tanVel(reward_data.reward_type == REWARD_YES);
    v_rew_clean = filter_outliers(v_rew_raw(~isnan(v_rew_raw)));
    mean_rew = mean(v_rew_clean);
    mean_rew_pct = (mean_rew/mean_no)*100;
    
    % Store in table
    temp = table({pat; pat}, {'No Reward'; 'Reward'}, [mean_no; mean_rew], [mean_no_pct; mean_rew_pct], ...
                 'VariableNames', {'SID', 'Condition', 'MeanPeakVel', 'MeanPeakVelPct'});
    subject_means = [subject_means; temp];
end

% Ensure correct categorical ordering
subject_means.Condition = categorical(subject_means.Condition, {'No Reward', 'Reward'});

%% --- 2. Plotting ---
figure('Color', 'w', 'Name', 'Subject Average Peak Velocity');
hold on;

% 1. Draw Boxplot
boxplot(subject_means.MeanPeakVel, subject_means.Condition, 'Symbol', '', 'Widths', 0.4);

% 2. Overlay Individual Participant Points and Paired Lines
% We extract the values into a matrix [N_subjects x 2] for easier line plotting
unique_sids = unique(subject_means.SID);
plot_data = zeros(length(unique_sids), 2);

for i = 1:length(unique_sids)
    subject_means.SID = string(subject_means.SID);
    idx_no = subject_means.SID == unique_sids{i} & subject_means.Condition == 'No Reward';
    idx_rew = subject_means.SID == unique_sids{i} & subject_means.Condition == 'Reward';

    plot_data(i, 1) = subject_means.MeanPeakVel(idx_no);
    plot_data(i, 2) = subject_means.MeanPeakVel(idx_rew);

    % Draw the connecting line for this participant
    plot([1, 2], [plot_data(i,1), plot_data(i,2)], '-', 'Color', [0.7 0.7 0.7], 'LineWidth', 1);
end

% 3. Draw the points on top of the lines
scatter(ones(size(plot_data,1),1), plot_data(:,1), 60, 'filled', 'MarkerFaceColor', [0.5 0.5 0.5], 'MarkerEdgeColor', 'k');
scatter(2*ones(size(plot_data,1),1), plot_data(:,2), 60, 'filled', 'MarkerFaceColor', [0.1 0.6 0.2], 'MarkerEdgeColor', 'k');

% 4. Formatting
ylabel('Peak Velocity (m/s)');
title(sprintf('Reward Effect on Peak Velocity (N=%d)', length(unique_sids)));
set(gca, 'XTick', [1 2], 'XTickLabel', {'No Reward', 'Reward'}, 'FontSize', 12);
xlim([0.5 2.5]);
grid on;

%% --- 2. Plotting ---
figure('Color', 'w', 'Name', 'Subject Average Peak Velocity');
hold on;

% 1. Draw Boxplot
boxplot(subject_means.MeanPeakVelPct, subject_means.Condition, 'Symbol', '', 'Widths', 0.4);

% 2. Overlay Individual Participant Points and Paired Lines
% We extract the values into a matrix [N_subjects x 2] for easier line plotting
unique_sids = unique(subject_means.SID);
plot_data = zeros(length(unique_sids), 2);

for i = 1:length(unique_sids)
    subject_means.SID = string(subject_means.SID);
    idx_no = subject_means.SID == unique_sids{i} & subject_means.Condition == 'No Reward';
    idx_rew = subject_means.SID == unique_sids{i} & subject_means.Condition == 'Reward';
    
    plot_data(i, 1) = subject_means.MeanPeakVelPct(idx_no);
    plot_data(i, 2) = subject_means.MeanPeakVelPct(idx_rew);
    
    % Draw the connecting line for this participant
    plot([1, 2], [plot_data(i,1), plot_data(i,2)], '-', 'Color', [0.7 0.7 0.7], 'LineWidth', 1);
end

% 3. Draw the points on top of the lines
scatter(ones(size(plot_data,1),1), plot_data(:,1), 60, 'filled', 'MarkerFaceColor', [0.5 0.5 0.5], 'MarkerEdgeColor', 'k');
scatter(2*ones(size(plot_data,1),1), plot_data(:,2), 60, 'filled', 'MarkerFaceColor', [0.1 0.6 0.2], 'MarkerEdgeColor', 'k');

% 4. Formatting
ylabel('Peak Velocity (m/s)');
title(sprintf('Reward Effect on Peak Velocity (N=%d)', length(unique_sids)));
set(gca, 'XTick', [1 2], 'XTickLabel', {'No Reward', 'Reward'}, 'FontSize', 12);
xlim([0.5 2.5]);
ylim([90 130]);
grid on;