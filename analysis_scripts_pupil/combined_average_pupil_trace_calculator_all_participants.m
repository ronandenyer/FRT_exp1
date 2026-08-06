%% Define Directory and Participants
% Note: Update the participant list and base_dir for your new combined dataset
 
participants = {'FRTC01','FRTC02', 'FRTC03', 'FRTC04'}; % Update with your 3 subjects
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\Combined_experiment\'; 

% Pre-allocate a cell array to store each subject's average table
super_grand_avg = cell(length(participants), 1);

%% Outer Loop: Iterate through each participant
for p = 1:length(participants)
    pat = participants{p};
    fprintf('Processing Subject: %s (%d of %d)\n', pat, p, length(participants));
    
    pat_dir = fullfile(base_dir, pat);
    data_file = fullfile(pat_dir, [pat, '_combined_reach_data_compact.mat']); 
    
    % Check if file exists before loading
    if ~exist(data_file, 'file')
        warning('File not found for %s, skipping...', pat);
        continue;
    end
    
    load(data_file); 
    combined_data = Compact_all;
    
    %% Make table to store average values for THIS subject
    pupil_avg = table();
    pupil_avg.SID(1:4) = string(pat);
    pupil_avg.condition_id = (1:4)';
    pupil_avg.cond_name = {'Sham_NoRew', 'Sham_Rew', 'tVNS_NoRew', 'tVNS_Rew'}';
    
    % Define the target values for each condition block
    target_stim = [1, 1, 2, 2];
    target_rew  = [3, 4, 3, 4];
    
    %% Inner Loop: Split by the 4 combinations of stim and reward
    for i = 1:height(pupil_avg)
        matchIdx = (combined_data.stim_type == target_stim(i)) & ...
                   (combined_data.reward_type == target_rew(i)) & ...
                   (combined_data.pupil_nan_percent < 0.3);
        
        fields = {'pupil_size_normalized', 'pupil_size'};
        
        for f = 1:length(fields)
            currentField = fields{f};
            cellArrays = combined_data.(currentField)(matchIdx);
            
            nonEmptyCells = cellfun(@(x) x', cellArrays(~cellfun(@isempty, cellArrays) & ...
                                       ~cellfun(@(x) all(isnan(x)), cellArrays)), ...
                                       'UniformOutput', false);
            
            if isempty(nonEmptyCells)
                pupil_avg.(currentField){i} = NaN(1, 12001); 
                continue; 
            end
            
            numericData = cell2mat(nonEmptyCells);
            trialMeans = mean(numericData, 2);
            outlierIdx = isoutlier(trialMeans); 
            
            numericData = numericData(~outlierIdx, :);
            pupil_avg.(currentField){i} = mean(numericData, 1);
        end
    end
    
    super_grand_avg{p} = pupil_avg;
end

%% Create Super Grand Average Table
master_table = vertcat(super_grand_avg{:});
save(fullfile(base_dir, 'pupil_grand_average_combined_all_pats.mat'), "master_table");

%% Save in Letswave format (4 conditions)
sham_norew_table = master_table(master_table.condition_id == 1, :);
sham_rew_table   = master_table(master_table.condition_id == 2, :);
tvns_norew_table = master_table(master_table.condition_id == 3, :);
tvns_rew_table   = master_table(master_table.condition_id == 4, :);

extract_mat = @(tbl, col) cell2mat(tbl.(col)(cellfun(@(x) ~any(isnan(x)), tbl.(col))));

sham_norew_mat_norm = extract_mat(sham_norew_table, 'pupil_size_normalized');
sham_norew_mat_raw  = extract_mat(sham_norew_table, 'pupil_size');

sham_rew_mat_norm = extract_mat(sham_rew_table, 'pupil_size_normalized');
sham_rew_mat_raw  = extract_mat(sham_rew_table, 'pupil_size');

tvns_norew_mat_norm = extract_mat(tvns_norew_table, 'pupil_size_normalized');
tvns_norew_mat_raw  = extract_mat(tvns_norew_table, 'pupil_size');

tvns_rew_mat_norm = extract_mat(tvns_rew_table, 'pupil_size_normalized');
tvns_rew_mat_raw  = extract_mat(tvns_rew_table, 'pupil_size');

save(fullfile(base_dir, 'letswave_format_pupil_data.mat'), ...
    'sham_norew_mat_norm', 'sham_norew_mat_raw', ...
    'sham_rew_mat_norm', 'sham_rew_mat_raw', ...
    'tvns_norew_mat_norm', 'tvns_norew_mat_raw', ...
    'tvns_rew_mat_norm', 'tvns_rew_mat_raw');

%% 1. Aggregate Across All Subjects
super_grand_avg = table();
cond_names = {'Sham_NoRew', 'Sham_Rew', 'tVNS_NoRew', 'tVNS_Rew'};

for c = 1:4
    cond_rows = master_table(master_table.condition_id == c, :);
    
    valid_idx = cellfun(@(x) ~any(isnan(x)), cond_rows.pupil_size_normalized);
    norm_matrix = cell2mat(cond_rows.pupil_size_normalized(valid_idx));
    raw_matrix = cell2mat(cond_rows.pupil_size(valid_idx));
    
    n_subs = size(norm_matrix, 1); 
    
    super_grand_avg.condition_id(c) = c;
    super_grand_avg.cond_name{c} = cond_names{c};
    
    super_grand_avg.pupil_size_normalized{c} = mean(norm_matrix, 1);
    super_grand_avg.pupil_size{c} = mean(raw_matrix, 1);
    
    super_grand_avg.sem_normalized{c} = std(norm_matrix, 0, 1) ./ sqrt(n_subs);
    super_grand_avg.sem{c} = std(raw_matrix, 0, 1) ./ sqrt(n_subs);
end

%% 3. Plotting: Side-by-Side Super Grand Averages
colors = { ...
    [0.1 0.1 0.1],  ... % Gray
    [0.2 0.6 0.86], ... % Blue
    [0 0.7 0],      ... % Green
    [0.9 0.3 0.24]  ... % Red
};

labels_legend = {'Sham + No Reward', 'Sham + Reward', 'tVNS + No Reward', 'tVNS + Reward'};

fig = figure('Color', 'w', 'Position', [100, 100, 1200, 500]);
t = tiledlayout(1, 2, 'TileSpacing', 'Compact', 'Padding', 'Compact');

fs = 1000; 
events = [2000, 4000, 5500, 6000];
labels = {'Stim onset', 'Prep tone 1', 'Prep tone 4', 'Stim offset'};

%% --- LEFT TILE: Normalized Pupil Size ---
nexttile;
hold on;
for c = 1:4
    mu = super_grand_avg.pupil_size_normalized{c};
    sem = super_grand_avg.sem_normalized{c};
    
    % DYNAMIC TIME VECTOR FIX: Ensure lengths match exactly
    time_vec = ((0:length(mu)-1) / fs) - 2;
    
    fill([time_vec, fliplr(time_vec)], [(mu + sem), fliplr(mu - sem)], ...
         colors{c}, 'FaceAlpha', 0.15, 'EdgeColor', 'none', 'HandleVisibility', 'off');
    
    plot(time_vec, mu, '-', 'Color', colors{c}, 'LineWidth', 2, 'DisplayName', labels_legend{c});
end
title('Normalized Pupil Size');
ylabel('Z-score / Ratio');
grid on; xlabel('Time (seconds)');
legend('Location', 'northwest', 'FontSize', 9);

yl = ylim;
% Rebuild a temporary standard time vector just to find event locations safely
event_time_vec = ((0:12000) / fs) - 2; 
for e = 1:length(events)
    line([event_time_vec(events(e)) event_time_vec(events(e))], yl, 'Color', [0.5 0.5 0.5], 'LineStyle', ':');
end
hold off;

%% --- RIGHT TILE: Raw Pupil Size ---
nexttile;
hold on;
for c = 1:4
    mu = super_grand_avg.pupil_size{c};
    sem = super_grand_avg.sem{c};
    
    % DYNAMIC TIME VECTOR FIX
    time_vec = ((0:length(mu)-1) / fs) - 2;
    
    fill([time_vec, fliplr(time_vec)], [(mu + sem), fliplr(mu - sem)], ...
         colors{c}, 'FaceAlpha', 0.15, 'EdgeColor', 'none', 'HandleVisibility', 'off');
    
    plot(time_vec, mu, '-', 'Color', colors{c}, 'LineWidth', 2, 'DisplayName', labels_legend{c});
end
title('Raw Pupil Size');
ylabel('Arbitrary Units');
grid on; xlabel('Time (seconds)');

yl_raw = ylim;
for e = 1:length(events)
    line([event_time_vec(events(e)) event_time_vec(events(e))], yl_raw, 'Color', [0.5 0.5 0.5], 'LineStyle', ':');
end
hold off;
title(t, '2x2 Pupil Dynamics Super Grand Average', 'FontSize', 14, 'FontWeight', 'Bold');

%% 4. Single Large Figure (Normalized Only)
figure('Color', 'w', 'Position', [200, 200, 800, 500]);
hold on;

for c = 1:4
    % DYNAMIC TIME VECTOR FIX
    mu = super_grand_avg.pupil_size_normalized{c};
    sem = super_grand_avg.sem_normalized{c};
    
    time_vec = ((0:length(mu)-1) / fs) - 2;
    
    x_fill = [time_vec, fliplr(time_vec)];
    y_fill = [(mu + sem), fliplr(mu - sem)];
    
    fill(x_fill, y_fill, colors{c}, 'FaceAlpha', 0.15, 'EdgeColor', 'none', 'HandleVisibility', 'off');
    plot(time_vec, mu, '-', 'Color', colors{c}, 'LineWidth', 2.5, 'DisplayName', labels_legend{c});
end

yl = ylim; 
for e = 1:length(events)
    event_time = event_time_vec(events(e));
    line([event_time event_time], yl, 'Color', [0.5 0.5 0.5], 'HandleVisibility', 'off');
    
    text(event_time, yl(2), labels{e}, 'Rotation', 90, 'VerticalAlignment', 'bottom', ...
        'HorizontalAlignment', 'right', 'FontSize', 10, 'Color', [0.3 0.3 0.3]);
end

title('Combined 2x2 Experiment: Pupil Size Grand Average', 'FontSize', 14, 'FontWeight', 'Bold');
xlabel('Time (seconds)', 'FontSize', 12, 'FontWeight', 'Bold');
ylabel('Normalized Pupil Size', 'FontSize', 12, 'FontWeight', 'Bold');
legend('show', 'Location', 'northwest');
grid on;
set(gca, 'FontSize', 11);
hold off;