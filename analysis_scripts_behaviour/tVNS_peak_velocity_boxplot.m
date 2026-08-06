%% AGGREGATION SCRIPT: Mean Peak Velocity per Subject (tVNS)
participants = {'FRTP01', 'FRTP02', 'FRTP03', 'FRTP04', 'FRTP05', 'FRTP06', ...
                'FRTP07', 'FRTP09', 'FRTP11', 'FRTP12', 'FRTP14', 'FRTP16', ...
                'FRTP17', 'FRTP18', 'FRTP20', 'FRTP21', 'FRTP22', 'FRTP23', ...
                'FRTP24', 'FRTP25', 'FRTP26', 'FRTP27', 'FRTP28', 'FRTP29', ...
                'FRTP30', 'FRTP31', 'FRTP32', 'FRTP33'};
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
   
    % Define outlier filter (1.5 * IQR)
    filter_outliers = @(d) d(d >= (quantile(d, 0.25) - 1.5*iqr(d)) & ...
                             d <= (quantile(d, 0.75) + 1.5*iqr(d)));
    
    % 1. Process Sham Mean
    v_sham_raw = data.peak_tanVel(data.stim_type == SHAM);
    v_sham_clean = filter_outliers(v_sham_raw(~isnan(v_sham_raw)));
    mean_sham = mean(v_sham_clean);
    
    % 2. Process Active (tVNS) Mean
    v_active_raw = data.peak_tanVel(data.stim_type == ACTIVE);
    v_active_clean = filter_outliers(v_active_raw(~isnan(v_active_raw)));
    mean_active = mean(v_active_clean);
    
    % Store in table (using SID as a string for compatibility)
    temp = table(string({pat; pat}), {'Sham'; 'Active tVNS'}, [mean_sham; mean_active], ...
                 'VariableNames', {'SID', 'Condition', 'MeanPeakVel'});
    subject_means_tvns = [subject_means_tvns; temp];
end

% Ensure correct categorical ordering for the x-axis
subject_means_tvns.Condition = categorical(subject_means_tvns.Condition, {'Sham', 'Active tVNS'});

%% --- 2. Plotting ---
figure('Color', 'w', 'Name', 'tVNS Subject Average Peak Velocity');
hold on;

% 1. Draw Boxplot
boxplot(subject_means_tvns.MeanPeakVel, subject_means_tvns.Condition, 'Symbol', '', 'Widths', 0.4);

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
ylim([0 1.43]);
grid on;


fprintf('Plotting complete. Calculated means for %d subjects.\n', length(unique_sids));