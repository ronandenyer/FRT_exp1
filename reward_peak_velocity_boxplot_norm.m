%% AGGREGATION SCRIPT: Mean Peak Velocity per Subject (reward)
participants = {'FRTP01', 'FRTP02', 'FRTP03', 'FRTP04', 'FRTP05', 'FRTP06', ...
                'FRTP07', 'FRTP09', 'FRTP11', 'FRTP12', ...
                'FRTP14', 'FRTP16', 'FRTP17', 'FRTP18', 'FRTP20', 'FRTP21',  ...
                'FRTP22','FRTP23', 'FRTP24', 'FRTP25', 'FRTP26', 'FRTP27', 'FRTP28', ...
                'FRTP29', 'FRTP30', 'FRTP31', 'FRTP32', 'FRTP33'};
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\Reward_experiment'; 

% Initialize table to store one row per participant per condition
subject_means_reward = table();

fprintf('Calculating subject-level means for reward experiment...\n');

for p = 1:length(participants)
    pat = participants{p};
    pat_file = fullfile(base_dir, pat, [pat, '_reward_reach_data_analyzed.mat']); 
    
    if ~exist(pat_file, 'file')
        warning('File not found for %s', pat);
        continue; 
    end
    
    L = load(pat_file);
    data = L.reward_data; % Note: loading the specific variable from your snippet
    
    No_REWARD = 3;
    ACTIVE = 2;

    % Load free response data
    pat_file_free = fullfile(base_dir, pat, [pat, '_free_response_reach_data_compact.mat']);
    load(pat_file_free);
    

    filter_outliers = @(data) data(data >= (quantile(data, 0.25) - 1.5*iqr(data)) & ...
                                   data <= (quantile(data, 0.75) + 1.5*iqr(data)));

    peak_tanVel_clean = filter_outliers(Compact.peak_tanVel(~isnan(Compact.peak_tanVel)));

    mean_free_peakVel = mean(peak_tanVel_clean);

    % --- 1. Calculate Velocity Delta ---
    reward = 4;
    NO_REWARD = 3;
   
    
    
    vel_NO_REWARD_raw  = data.peak_tanVel(data.reward_type == NO_REWARD);
    vel_reward_raw = data.peak_tanVel(data.reward_type == reward);
    
    vel_NO_REWARD_clean  = filter_outliers(vel_NO_REWARD_raw(~isnan(vel_NO_REWARD_raw)));
    vel_reward_clean = filter_outliers(vel_reward_raw(~isnan(vel_reward_raw)));
    
    vel_NO_REWARD_pct = vel_NO_REWARD_clean./mean_free_peakVel;
    vel_reward_pct = vel_reward_clean./mean_free_peakVel;

    mean_vel_NO_REWARD = mean(vel_NO_REWARD_pct)*100;
    mean_vel_reward = mean(vel_reward_pct)*100;
   
    
    
    % Store in table (using SID as a string for compatibility)
    temp = table(string({pat; pat}), {'NO_REWARD'; 'REWARD'}, [mean_vel_NO_REWARD; mean_vel_reward], ...
                 'VariableNames', {'SID', 'Condition', 'MeanPeakVel'});
    subject_means_reward = [subject_means_reward; temp];
end

% Ensure correct categorical ordering for the x-axis
%subject_means_reward.Condition = categorical(subject_means_reward.Condition);
writetable(subject_means_reward, strcat(base_dir, '\', 'reward_mean_peak_velocity_norm.csv'));
%% --- 2. Plotting ---
subject_means_reward.Condition = categorical(subject_means_reward.Condition, {'NO_REWARD', 'Active reward'});
figure('Color', 'w', 'Name', 'reward Subject Average Peak Velocity');
hold on;

% 1. Draw Boxplot
boxplot(subject_means_reward.MeanPeakVel, subject_means_reward.Condition, 'Symbol', '', 'Widths', 0.4);

% 2. Calculate the means for each group
% (Assuming you already have the 'active' and 'NO_REWARD' variables from your script)
mean_NO_REWARD   = mean(NO_REWARD);
mean_active = mean(reward);

% 3. Draw the mean lines manually
% We use a short line centered at x=1 and x=2
line_width = 0.2; % How wide the mean line should be
line([1-line_width, 1+line_width], [mean_NO_REWARD, mean_NO_REWARD], 'Color', 'r', 'LineWidth', 2);
line([2-line_width, 2+line_width], [mean_active, mean_active], 'Color', 'r', 'LineWidth', 2);

% 2. Extract unique SIDs to draw paired lines
unique_sids = unique(subject_means_reward.SID);
plot_data = zeros(length(unique_sids), 2);

for i = 1:length(unique_sids)
    % Using strcmp to avoid cell array comparison errors
    idx_NO_REWARD   = strcmp(subject_means_reward.SID, unique_sids(i)) & (subject_means_reward.Condition == 'NO_REWARD');
    idx_active = strcmp(subject_means_reward.SID, unique_sids(i)) & (subject_means_reward.Condition == 'Active reward');
    
    plot_data(i, 1) = subject_means_reward.MeanPeakVel(idx_NO_REWARD);
    plot_data(i, 2) = subject_means_reward.MeanPeakVel(idx_active);
    
    % Draw the connecting line for this participant (light grey)
    plot([1, 2], [plot_data(i,1), plot_data(i,2)], '-', 'Color', [0.7 0.7 0.7], 'LineWidth', 1);
end


% 3. Draw the individual points on top
scatter(ones(size(plot_data,1),1), plot_data(:,1), 70, 'filled', 'MarkerFaceColor', [0.6 0.6 0.6], 'MarkerEdgeColor', 'k');
scatter(2*ones(size(plot_data,1),1), plot_data(:,2), 70, 'filled', 'MarkerFaceColor', [0.1 0.6 0.1], 'MarkerEdgeColor', 'k');

% 4. Formatting
ylabel('Mean Peak Velocity (m/s)');
title(sprintf('reward Effect on Peak Velocity (N=%d)', length(unique_sids)));
set(gca, 'XTick', [1 2], 'XTickLabel', {'NO_REWARD', 'Active reward'}, 'FontSize', 12);
xlim([0.5 2.5]);
% ylim([0 1.43]);
grid on;


fprintf('Plotting complete. Calculated means for %d subjects.\n', length(unique_sids));

% 1. Extract the numeric values for 'Active reward'
active = subject_means_reward.MeanPeakVel(subject_means_reward.Condition == 'Active reward');

% 2. Extract the numeric values for 'NO_REWARD'
NO_REWARD = subject_means_reward.MeanPeakVel(subject_means_reward.Condition == 'NO_REWARD');

% 3. Run the paired t-test on the two numeric vectors
[h, p, ci, stats] = ttest(active, NO_REWARD);

% Display results
fprintf('t(%d) = %.3f, p = %.4f\n', stats.df, stats.tstat, p);