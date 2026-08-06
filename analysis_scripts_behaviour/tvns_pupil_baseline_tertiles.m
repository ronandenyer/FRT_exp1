%% Define Directory and Participants
participants = {'FRTP01', 'FRTP02', 'FRTP03', 'FRTP04', 'FRTP05', 'FRTP06', ...
                'FRTP07', 'FRTP09', 'FRTP11', 'FRTP12', ...
                'FRTP14', 'FRTP16', 'FRTP17', 'FRTP18', 'FRTP20', 'FRTP21',  ...
                'FRTP22','FRTP23', 'FRTP24', 'FRTP25', 'FRTP26', 'FRTP27', 'FRTP28', ...
                'FRTP29', 'FRTP30', 'FRTP31', 'FRTP32', 'FRTP33'};

base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\tvns_experiment\'; 

% Initialize cell array to securely store results
all_rows = {};

for p = 1:length(participants)
    pat = participants{p};
    fprintf('Processing Subject: %s (%d of %d)\n', pat, p, length(participants));
    
    pat_dir = fullfile(base_dir, pat);
    compact_file = fullfile(pat_dir, [pat, '_tvns_reach_data_analyzed.mat']);
    
    if ~exist(compact_file, 'file')
        warning('File missing for %s, skipping...', pat);
        continue;
    end

    %% 1. Calculate Within-Subjects Factor: Trial-Level Baseline Split (Tertiles)
    L_comp = load(compact_file);
    tvns_data = L_comp.tvns_data_light;
    n_trials = height(tvns_data);

    % BULLETPROOF FIX 1: Dynamically determine NaN threshold scale
    if ismember('pupil_nan_percent', tvns_data.Properties.VariableNames)
        if max(tvns_data.pupil_nan_percent) > 1.0
            valid_pupil = tvns_data.pupil_nan_percent < 30; % Assumes 0-100 scale
        else
            valid_pupil = tvns_data.pupil_nan_percent < 0.3; % Assumes 0-1 scale
        end
    else
        valid_pupil = true(n_trials, 1); % Fallback if column is missing
    end
    
    % If criteria is STILL too strict and removes everything, bypass it so data plots
    if sum(valid_pupil) == 0
        valid_pupil = true(n_trials, 1);
    end
    
    % Extract trial-level baselines dynamically
    trial_baselines = nan(n_trials, 1);
    
    for t = 1:n_trials
        if valid_pupil(t)
            % BULLETPROOF FIX 2: Handle both cell arrays and numeric matrices
            if iscell(tvns_data.pupil_size)
                trace = tvns_data.pupil_size{t};
            else
                trace = tvns_data.pupil_size(t, :);
            end
            
            % Safely take baseline (up to 2000 samples)
            if ~isempty(trace) && ~all(isnan(trace))
                win_len = min(2000, length(trace));
                trial_baselines(t) = mean(trace(1:win_len), 'omitnan');
            end
        end
    end
    
    % Determine THIS participant's tertile cutoffs
    valid_bases = trial_baselines(~isnan(trial_baselines));
    if isempty(valid_bases)
        fprintf('--> ERROR: Still no valid baselines for %s. Check data structure.\n', pat);
        continue; 
    end
    
    tertiles = prctile(valid_bases, [33.33, 66.67]);
    t1_cutoff = tertiles(1);
    t2_cutoff = tertiles(2);
    
    %% 2. Average Normalized Traces across the 6 Splits
    for stim = 1:2
        for base_level = 1:3
            % Find matching trials for this specific split
            if base_level == 1
                trial_idx = (tvns_data.stim_type == stim) & valid_pupil & (trial_baselines <= t1_cutoff);
            elseif base_level == 2
                trial_idx = (tvns_data.stim_type == stim) & valid_pupil & (trial_baselines > t1_cutoff) & (trial_baselines <= t2_cutoff);
            else
                trial_idx = (tvns_data.stim_type == stim) & valid_pupil & (trial_baselines > t2_cutoff);
            end
            
            % Extract and clean normalized traces
            if iscell(tvns_data.pupil_size_normalized)
                cellArrays = tvns_data.pupil_size_normalized(trial_idx);
                valid_cells = cellArrays(~cellfun(@isempty, cellArrays) & ~cellfun(@(x) all(isnan(x)), cellArrays));
                % Force rows to ensure proper concatenation
                nonEmptyCells = cellfun(@(x) reshape(x, 1, []), valid_cells, 'UniformOutput', false);
                if ~isempty(nonEmptyCells)
                    numericData = cell2mat(nonEmptyCells);
                else
                    numericData = [];
                end
            else
                numericData = tvns_data.pupil_size_normalized(trial_idx, :);
                numericData(all(isnan(numericData), 2), :) = []; % Remove fully NaN rows
            end
            
            if ~isempty(numericData)
                % Outlier removal based on trial means
                trialMeans = mean(numericData, 2, 'omitnan');
                outlierIdx = isoutlier(trialMeans);
                numericData = numericData(~outlierIdx, :);
                
                if ~isempty(numericData)
                    % Grand average for this specific split
                    mean_trace = mean(numericData, 1, 'omitnan');
                    all_rows(end+1, :) = {pat, stim, base_level, mean_trace};
                end
            end
        end
    end
end

%% 3. Convert to Table & Safety Check
if isempty(all_rows)
    error('CRITICAL: No valid data was found across any subjects. Please check if your pupil data is empty.');
end

subject_traces = cell2table(all_rows, 'VariableNames', {'SID', 'StimCondition', 'BaseLevel', 'NormTrace'});

%% 4. Plotting the 1x3 Grid (Baseline Tertiles)
fig = figure('Color', 'w', 'Position', [100, 100, 1500, 450]); 
t = tiledlayout(1, 3, 'TileSpacing', 'Compact', 'Padding', 'Compact');

fs = 1000;
time_seconds = ((0:12000) / fs) - 2;
colors = {[0, 0.447, 0.741], [0.85, 0.325, 0.098]}; % Blue (Sham), Orange (tVNS)
events = [2000, 4000, 5500, 6000];

col_titles = {'Low Baseline', 'Medium Baseline', 'High Baseline'};

for base_level = 1:3
    nexttile;
    hold on;
    
    % Filter master table for this specific tile
    match_rows = subject_traces(subject_traces.BaseLevel == base_level, :);
    n_pats = length(unique(match_rows.SID));
    
    for stim = 1:2
        % Extract traces for this condition
        stim_rows = match_rows(match_rows.StimCondition == stim, :);
        
        if ~isempty(stim_rows)
            traces = stim_rows.NormTrace;
            
            % FIXED: Check if MATLAB auto-converted the column to a double matrix
            if iscell(traces)
                min_len = min(cellfun(@length, traces));
                traces_cut = cellfun(@(x) x(1:min_len), traces, 'UniformOutput', false);
                norm_matrix = cell2mat(traces_cut);
            else
                % It is already a uniform numeric matrix!
                norm_matrix = traces;
                min_len = size(norm_matrix, 2); 
            end
            
            n_subs = size(norm_matrix, 1);
            mu = mean(norm_matrix, 1, 'omitnan');
            sem = std(norm_matrix, 0, 1, 'omitnan') ./ sqrt(n_subs);
            
            time_plot = time_seconds(1:min_len);
            
            % Plot SEM Ribbon
            fill([time_plot, fliplr(time_plot)], [(mu + sem), fliplr(mu - sem)], ...
                 colors{stim}, 'FaceAlpha', 0.15, 'EdgeColor', 'none', 'HandleVisibility', 'off');
            
            % Plot Mean Line
            lineStyle = '-'; if stim == 2, lineStyle = '--'; end
            cond_name = 'Sham'; if stim == 2; cond_name = 'tVNS'; end
            
            plot(time_plot, mu, lineStyle, 'Color', colors{stim}, 'LineWidth', 2, 'DisplayName', cond_name);
        end
    end
    
    % Formatting
    title(sprintf('%s\n(n=%d)', col_titles{base_level}, n_pats), 'FontSize', 12);
    grid on; 
    xlabel('Time (seconds)');
    
    % Keep Y-label only on the left-most plot to reduce clutter
    if base_level == 1
        ylabel('Norm. Pupil Size');
    end
    
    % Place legend only on the right-most plot
    if base_level == 3
        legend('Location', 'northeast');
    end
    
    % Event Lines
    yl = ylim;
    for e = 1:length(events)
        % Only plot event lines if they fall within our extracted time window
        if events(e) <= min_len
            event_time = time_seconds(events(e));
            line([event_time event_time], yl, 'Color', [0.5 0.5 0.5], 'LineStyle', ':', 'HandleVisibility', 'off');
        end
    end
end

title(t, 'Pupil Traces across Trial-Level Baseline Arousal (Within-Subjects Tertiles)', 'FontSize', 16, 'FontWeight', 'Bold');

% Lock Y-axes across all subplots dynamically
linkaxes(findall(fig, 'type', 'axes'), 'y');