%% Define Directory and Participants
participants = {'FRTP01', 'FRTP02', 'FRTP03', 'FRTP04', 'FRTP05', 'FRTP06', ...
                'FRTP07', 'FRTP09', 'FRTP11', 'FRTP12', ...
                'FRTP14', 'FRTP16', 'FRTP17', 'FRTP18', 'FRTP20', 'FRTP21',  ...
                'FRTP22','FRTP23', 'FRTP24', 'FRTP25', 'FRTP26', 'FRTP27', 'FRTP28', ...
                'FRTP29', 'FRTP30', 'FRTP31', 'FRTP32', 'FRTP33'};

base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\tvns_experiment\'; 

% Pre-allocate a cell array to store each subject's average table
super_grand_avg = cell(length(participants), 1);

%% Outer Loop: Iterate through each participant
for p = 1:length(participants)
    pat = participants{p};
    fprintf('Processing Subject: %s (%d of %d)\n', pat, p, length(participants));
    
    pat_dir = fullfile(base_dir, pat);
    tvns_file = fullfile(pat_dir, [pat, '_tvns_reach_data_compact.mat']);
    
    % Check if file exists before loading
    if ~exist(tvns_file, 'file')
        warning('File not found for %s, skipping...', pat);
        continue;
    end
    
    load(tvns_file); % Loads Compact_all
    tvns_data = Compact_all;

    %% Make table to store average values for THIS subject
    pupil_avg = table();
    pupil_avg.SID(1:2) = string(pat);
    pupil_avg.stim_condition(1) = 1;
    pupil_avg.stim_condition(2) = 2;

    %% Inner Loop: Split by stim condition
    for i = 1:height(pupil_avg)
        matchIdx = tvns_data.stim_type == pupil_avg.stim_condition(i) & tvns_data.pupil_nan_percent < 0.3;
        
        % Data types to process
        fields = {'pupil_size_normalized', 'pupil_size'};
        
        for f = 1:length(fields)
            currentField = fields{f};
            cellArrays = tvns_data.(currentField)(matchIdx);
            
            % Clean empty/NaN cells
            nonEmptyCells = cellfun(@(x) x', cellArrays(~cellfun(@isempty, cellArrays) & ...
                                       ~cellfun(@(x) all(isnan(x)), cellArrays)), ...
                                       'UniformOutput', false);
            
            if isempty(nonEmptyCells), continue; end
            
            numericData = cell2mat(nonEmptyCells);
            trialMeans = mean(numericData, 2);
            outlierIdx = isoutlier(trialMeans); 
            
            % Remove outliers and average
            numericData = numericData(~outlierIdx, :);
            pupil_avg.(currentField){i} = mean(numericData, 1);
        end
    end
    
    % Store this subject's result in our master list
    super_grand_avg{p} = pupil_avg;
end

%% Create Super Grand Average
master_table = vertcat(super_grand_avg{:});
save(strcat(base_dir, 'pupil_grand_average_tvns_all_pats.mat'),"master_table");

%% save in letswave format
table_sham = master_table(master_table.stim_condition == 1,:);
table_tvns = master_table(master_table.stim_condition == 2,:);

sham_mat = zeros(height(table_sham), 12001);
tvns_mat = zeros(height(table_tvns), 12001);
sham_mat_raw = zeros(height(table_sham), 12001);
tvns_mat_raw = zeros(height(table_tvns), 12001);

for a = 1:height(sham_mat)
    pupil = table_sham.pupil_size_normalized{a};
    pupil_raw = table_sham.pupil_size{a};

    sham_mat(a,:) = pupil;
    sham_mat_raw(a,:) = pupil_raw;

end

for a = 1:height(tvns_mat)
    pupil = table_tvns.pupil_size_normalized{a};
    pupil_raw = table_tvns.pupil_size{a};
    tvns_mat(a,:) = pupil;
    tvns_mat_raw(a,:) = pupil_raw;

end


save(strcat(base_dir, '\', "letswave_format_pupil_data.mat"), "sham_mat", "tvns_mat", "sham_mat_raw", "tvns_mat_raw");

%% 1. Aggregate Across All Subjects (The True Super Grand Average)
%%
super_grand_avg = table();
conditions = [1, 2];

for c = 1:length(conditions)
    cond_rows = master_table(master_table.stim_condition == c, :);
    
    % Stack all subject averages (Rows = Subjects, Cols = Time)
    norm_matrix = cell2mat(cond_rows.pupil_size_normalized);
    raw_matrix = cell2mat(cond_rows.pupil_size);
    
    n_subs = size(norm_matrix, 1); % Number of subjects for SEM
    
    super_grand_avg.stim_condition(c) = conditions(c);
    
    % Calculate Mean
    super_grand_avg.pupil_size_normalized{c} = mean(norm_matrix, 1);
    super_grand_avg.pupil_size{c} = mean(raw_matrix, 1);
    % Calculate SEM: SD / sqrt(n)
    super_grand_avg.sem_normalized{c} = std(norm_matrix, 0, 1) ./ sqrt(n_subs);
    super_grand_avg.sem{c} = std(raw_matrix, 0, 1) ./ sqrt(n_subs);
end
%% Create Maximum Change Table (tVNS vs Sham)

sham_cond = 1;
tvns_cond = 2;
time_range = 2000:4000;
baseline_range = 1:2000;
unique_subs = unique(master_table.SID);
n_subs = length(unique_subs);

% 1. Pre-allocate the table with the correct number of rows

max_change_results = table(unique_subs, zeros(n_subs, 1), zeros(n_subs, 1), zeros(n_subs, 1), ...
'VariableNames', {'SID', 'Max_Diff', 'Peak_Index', 'Mean_Base_Diff'});

for s = 1:n_subs
sub_id = unique_subs(s);

% Extract rows for this subject

sub_data = master_table(master_table.SID == sub_id, :);


% Check if subject has both conditions

has_sham = any(sub_data.stim_condition == sham_cond);

has_tvns = any(sub_data.stim_condition == tvns_cond);


if has_sham && has_tvns

% Extract traces

sham_trace = sub_data.pupil_size_normalized{sub_data.stim_condition == sham_cond};

tvns_trace = sub_data.pupil_size_normalized{sub_data.stim_condition == tvns_cond};


% Calculate difference within range

diff_trace = tvns_trace(time_range) - sham_trace(time_range);


% Find absolute max change

[~, max_idx] = max(abs(diff_trace));


% Assign values to the pre-allocated row

max_change_results.Max_Diff(s) = diff_trace(max_idx);

max_change_results.Peak_Index(s) = max_idx + time_range(1) - 1;

% now find difference in raw data pre first cue
sham_trace_base = sub_data.pupil_size{sub_data.stim_condition == sham_cond};

tvns_trace_base = sub_data.pupil_size{sub_data.stim_condition == tvns_cond};

diff_trace_base = tvns_trace_base(baseline_range) - sham_trace_base(baseline_range);
max_change_results.Mean_Base_Diff(s) = mean(diff_trace, 2);

else

% If data is missing, mark as NaN

max_change_results.Max_Diff(s) = NaN;

max_change_results.Peak_Index(s) = NaN;

max_change_results.Mean_Base_Diff(s) = NaN;

end

end

%% Perform Median Split

% Calculate the median of the Max_Diff column, ignoring any NaNs

group_median = median(max_change_results.Max_Diff, 'omitnan');

% Create a new categorical column for the split

% 1 = Above Median, 0 = Below or Equal to Median

max_change_results.Split_Group = double(max_change_results.Max_Diff > group_median);
%% 3. Plotting: Side-by-Side Super Grand Averages
% Create a figure with a tiled layout (1 row, 2 columns)
fig = figure('Color', 'w', 'Position', [100, 100, 1200, 500]);
t = tiledlayout(1, 2, 'TileSpacing', 'Compact', 'Padding', 'Compact');

% Shared Plotting Parameters
fs = 1000; 
time_seconds = ((0:12000) / fs) - 2; 
colors = {[0, 0.447, 0.741], [0.85, 0.325, 0.098]}; % Blue (Sham), Red (tVNS)
events = [2000, 4000, 5500, 6000];
labels = {'Train onset', 'Prep tone 1', 'Prep tone 4', 'Train offset'};

%% --- LEFT TILE: Normalized Pupil Size ---
nexttile;
hold on;
for c = 1:2
    mu = super_grand_avg.pupil_size_normalized{c};
    sem = super_grand_avg.sem_normalized{c};
    
    fill([time_seconds, fliplr(time_seconds)], [(mu + sem), fliplr(mu - sem)], ...
         colors{c}, 'FaceAlpha', 0.15, 'EdgeColor', 'none', 'HandleVisibility', 'off');
    
    lineStyle = '-'; if c == 2, lineStyle = '--'; end
    plot(time_seconds, mu, lineStyle, 'Color', colors{c}, 'LineWidth', 2, ...
        'DisplayName', sprintf('Condition %d', c));
end
title('Normalized Pupil Size');
ylabel('Z-score / Ratio');
grid on; xlabel('Time (seconds)');
legend('Location', 'northeast');

% Add event lines to Left Tile
yl = ylim;
for e = 1:length(events)
    line([time_seconds(events(e)) time_seconds(events(e))], yl, 'Color', [0.5 0.5 0.5], 'LineStyle', ':');
end

% --- RIGHT TILE: Raw Pupil Size ---
nexttile;
hold on;
for c = 1:2
    mu = super_grand_avg.pupil_size{c};
    sem = super_grand_avg.sem{c};
    
    fill([time_seconds, fliplr(time_seconds)], [(mu + sem), fliplr(mu - sem)], ...
         colors{c}, 'FaceAlpha', 0.15, 'EdgeColor', 'none', 'HandleVisibility', 'off');
    
    lineStyle = '-'; if c == 2, lineStyle = '--'; end
    plot(time_seconds, mu, lineStyle, 'Color', colors{c}, 'LineWidth', 2, ...
        'DisplayName', sprintf('Condition %d', c));
end
title('Raw Pupil Size');
ylabel('Arbitrary Units (Pixels)');
grid on; xlabel('Time (seconds)');
legend('Location', 'northeast');

% Add event lines to Right Tile
yl_raw = ylim;
for e = 1:length(events)
    line([time_seconds(events(e)) time_seconds(events(e))], yl_raw, 'Color', [0.5 0.5 0.5], 'LineStyle', ':');
end
% Global Title for the whole figure
title(t, 'tVNS Super Grand Average: All Participants', 'FontSize', 14, 'FontWeight', 'Bold');
%% 
figure;
hold on;

% 1. Define Time Vector: Start at -2, 8000 samples total
% Assuming 1000 Hz sampling rate
fs = 1000; 
time_seconds = ((0:11999) / fs) - 2; 

colors = {'b', 'r'}; 
% Define event indices and their labels
events = [2000, 4000, 5500, 6000];
labels = {'Stim onset', 'Prep tone 1', 'Prep tone 4', 'Stim offset'};

% Plot Shaded Area and Mean Lines
for c = 1:2
    % Extract data
    mu = super_grand_avg.pupil_size_normalized{c}(1:12000);
    sem = super_grand_avg.sem_normalized{c}(1:12000);
    
    % Create coordinates for the shaded polygon using the NEW time_seconds
    x_fill = [time_seconds, fliplr(time_seconds)];
    y_fill = [(mu + sem), fliplr(mu - sem)];
    
    % Plot the shaded area
    fill(x_fill, y_fill, colors{c}, 'FaceAlpha', 0.2, 'EdgeColor', 'none', 'HandleVisibility', 'off');
    
    % Plot the mean line
    lineStyle = '-'; if c == 2, lineStyle = '--'; end
    if c ==1
        plot(time_seconds, mu, lineStyle, 'Color', colors{c}, 'LineWidth', 2, 'DisplayName', 'Sham');
    else
        plot(time_seconds, mu, lineStyle, 'Color', colors{c}, 'LineWidth', 2, 'DisplayName', 'tVNS');
    end
end

% 2. Add Vertical Event Lines and Vertical Text
yl = ylim; % Get current y-axis limits for text placement
for e = 1:length(events)
    % Convert sample index to the corresponding time in seconds
    event_time = time_seconds(events(e));
    
    % Draw the vertical line
    line([event_time event_time], yl, 'Color', [0.5 0.5 0.5], 'HandleVisibility', 'off');
    
    % Add text: rotated 90 degrees
    % 'VerticalAlignment', 'bottom' makes it sit to the right of the line when rotated
    text(event_time, yl(2), labels{e}, 'Rotation', 90, 'VerticalAlignment', 'bottom', ...
        'HorizontalAlignment', 'right', 'FontSize', 10, 'Color', [0.3 0.3 0.3]);
end

title('tVNS Experiment: Pupil Size Grand Average');
xlabel('Time (seconds)');
ylabel('Normalized Pupil Size');
legend('show', 'Location', 'best');
grid on;
hold off;

%% Create Subject-Level Difference Score Table: tVNS - Sham

sham_cond = 1;
tvns_cond = 2;

pre_cue_range = 1:2000;          % first 2000 values
post_cue_range = 2001:6001;      % values 2001 through 6001

unique_subs = unique(master_table.SID);
n_subs = length(unique_subs);

diff_score_results = table( ...
    unique_subs, ...
    NaN(n_subs, 1), ...
    NaN(n_subs, 1), ...
    NaN(n_subs, 1), ...
    'VariableNames', { ...
        'SID', ...
        'pre_cue_diff_score', ...
        'normalized_diff_mean_2001_6001', ...
        'normalized_diff_max_2001_6001' ...
    });

for s = 1:n_subs

    sub_id = unique_subs(s);

    % Extract this participant's data
    sub_data = master_table(master_table.SID == sub_id, :);

    % Check participant has both Sham and tVNS
    has_sham = any(sub_data.stim_condition == sham_cond);
    has_tvns = any(sub_data.stim_condition == tvns_cond);

    if has_sham && has_tvns

        %% Raw pupil_size difference: tVNS - Sham
        sham_raw = sub_data.pupil_size{sub_data.stim_condition == sham_cond};
        tvns_raw = sub_data.pupil_size{sub_data.stim_condition == tvns_cond};

        raw_diff = tvns_raw - sham_raw;

        % Mean difference score across first 2000 values
        diff_score_results.pre_cue_diff_score(s) = mean(raw_diff(pre_cue_range), 'omitnan');


        %% Normalized pupil_size difference: tVNS - Sham
        sham_norm = sub_data.pupil_size_normalized{sub_data.stim_condition == sham_cond};
        tvns_norm = sub_data.pupil_size_normalized{sub_data.stim_condition == tvns_cond};

        norm_diff = tvns_norm - sham_norm;

        % Mean difference from item 2001 to 6001
        diff_score_results.normalized_diff_mean_2001_6001(s) = ...
            mean(norm_diff(post_cue_range), 'omitnan');

        % Max difference from item 2001 to 6001
        diff_score_results.normalized_diff_max_2001_6001(s) = ...
            max(norm_diff(post_cue_range), [], 'omitnan');

    else

        warning('Missing Sham or tVNS data for %s. Setting values to NaN.', sub_id);

    end
end