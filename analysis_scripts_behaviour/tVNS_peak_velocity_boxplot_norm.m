%% AGGREGATION SCRIPT: Mean Peak Velocity per Subject (tVNS)
participants = {'FRTP01', 'FRTP02', 'FRTP03', 'FRTP04', 'FRTP05', 'FRTP06', ...
                'FRTP07', 'FRTP09', 'FRTP11', 'FRTP12', ...
                'FRTP14', 'FRTP16', 'FRTP17', 'FRTP18', 'FRTP20', 'FRTP21',  ...
                'FRTP22','FRTP23', 'FRTP24', 'FRTP25', 'FRTP26', 'FRTP27', 'FRTP28', ...
                'FRTP29', 'FRTP30', 'FRTP31', 'FRTP32', 'FRTP33'};
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\tvns_experiment'; 

% Initialize table to store one row per participant per condition
subject_means_tvns = table();

fprintf('Calculating subject-level means for tVNS experiment...\n');

for p = 1:length(participants)
    pat = participants{p};
    pat_file = fullfile(base_dir, pat, [pat, '_tvns_reach_data_analyzed.mat']); 
    
    if ~exist(pat_file, 'file')
        warning('File not found for %s', pat);
        continue; 
    end
    
    L = load(pat_file);
    data = L.tvns_data_light; % Note: loading the specific variable from your snippet
    
    SHAM = 1;
    ACTIVE = 2;

    % Load free response data
    pat_file_free = fullfile(base_dir, pat, [pat, '_free_response_reach_data_compact.mat']);
    load(pat_file_free);
    

    filter_outliers = @(data) data(data >= (quantile(data, 0.25) - 1.5*iqr(data)) & ...
                                   data <= (quantile(data, 0.75) + 1.5*iqr(data)));

    peak_tanVel_clean = filter_outliers(Compact.peak_tanVel(~isnan(Compact.peak_tanVel)));

    mean_free_peakVel = mean(peak_tanVel_clean);

    % --- 1. Calculate Velocity Delta ---
    tvns = 2;
    sham = 1;
   
    
    
    vel_sham_raw  = data.peak_tanVel(data.stim_type == sham);
    vel_tvns_raw = data.peak_tanVel(data.stim_type == tvns);
    
    vel_sham_clean  = filter_outliers(vel_sham_raw(~isnan(vel_sham_raw)));
    vel_tvns_clean = filter_outliers(vel_tvns_raw(~isnan(vel_tvns_raw)));
    
    vel_sham_pct = vel_sham_clean./mean_free_peakVel;
    vel_tvns_pct = vel_tvns_clean./mean_free_peakVel;

    mean_vel_sham = mean(vel_sham_pct)*100;
    mean_vel_tvns = mean(vel_tvns_pct)*100;
   
    
    
    % Store in table (using SID as a string for compatibility)
    temp = table(string({pat; pat}), {'Sham'; 'tVNS'}, [mean_vel_sham; mean_vel_tvns], ...
                 'VariableNames', {'SID', 'Condition', 'MeanPeakVel'});
    subject_means_tvns = [subject_means_tvns; temp];
end

% Ensure correct categorical ordering for the x-axis
subject_means_tvns.Condition = categorical(subject_means_tvns.Condition);
writetable(subject_means_tvns, strcat(base_dir, '\', 'tvns_mean_peak_velocity_norm.csv'));

%% --- 2. Plotting ---
subject_means_tvns.Condition = categorical(subject_means_tvns.Condition, {'Sham', 'Active tVNS'});
figure('Color', 'w', 'Name', 'tVNS Subject Average Peak Velocity');
hold on;

% 1. Draw Boxplot
boxplot(subject_means_tvns.MeanPeakVel, subject_means_tvns.Condition, 'Symbol', '', 'Widths', 0.4);

% 2. Calculate the means for each group
% (Assuming you already have the 'active' and 'sham' variables from your script)
mean_sham   = mean(sham);
mean_active = mean(active);

% 3. Draw the mean lines manually
% We use a short line centered at x=1 and x=2
line_width = 0.2; % How wide the mean line should be
line([1-line_width, 1+line_width], [mean_sham, mean_sham], 'Color', 'r', 'LineWidth', 2);
line([2-line_width, 2+line_width], [mean_active, mean_active], 'Color', 'r', 'LineWidth', 2);

% 2. Extract unique SIDs to draw paired lines
unique_sids = unique(subject_means_tvns.SID);
plot_data = zeros(length(unique_sids), 2);

for i = 1:length(unique_sids)
    % Using strcmp to avoid cell array comparison errors
    idx_sham   = strcmp(subject_means_tvns.SID, unique_sids(i)) & (subject_means_tvns.Condition == 'Sham');
    idx_active = strcmp(subject_means_tvns.SID, unique_sids(i)) & (subject_means_tvns.Condition == 'Active tVNS');
    
    plot_data(i, 1) = subject_means_tvns.MeanPeakVel(idx_sham);
    plot_data(i, 2) = subject_means_tvns.MeanPeakVel(idx_active);
    
    % Draw the connecting line for this participant (light grey)
    plot([1, 2], [plot_data(i,1), plot_data(i,2)], '-', 'Color', [0.7 0.7 0.7], 'LineWidth', 1);
end


% 3. Draw the individual points on top
scatter(ones(size(plot_data,1),1), plot_data(:,1), 70, 'filled', 'MarkerFaceColor', [0.6 0.6 0.6], 'MarkerEdgeColor', 'k');
scatter(2*ones(size(plot_data,1),1), plot_data(:,2), 70, 'filled', 'MarkerFaceColor', [0.1 0.6 0.1], 'MarkerEdgeColor', 'k');

% 4. Formatting
ylabel('Mean Peak Velocity (m/s)');
title(sprintf('tVNS Effect on Peak Velocity (N=%d)', length(unique_sids)));
set(gca, 'XTick', [1 2], 'XTickLabel', {'Sham', 'Active tVNS'}, 'FontSize', 12);
xlim([0.5 2.5]);
% ylim([0 1.43]);
grid on;


fprintf('Plotting complete. Calculated means for %d subjects.\n', length(unique_sids));

% 1. Extract the numeric values for 'Active tVNS'
active = subject_means_tvns.MeanPeakVel(subject_means_tvns.Condition == 'Active tVNS');

% 2. Extract the numeric values for 'Sham'
sham = subject_means_tvns.MeanPeakVel(subject_means_tvns.Condition == 'Sham');

% 3. Run the paired t-test on the two numeric vectors
[h, p, ci, stats] = ttest(active, sham);

% Display results
fprintf('t(%d) = %.3f, p = %.4f\n', stats.df, stats.tstat, p);