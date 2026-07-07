%% Define Directory and Participants
participants = {'FRTP01', 'FRTP02', 'FRTP03', 'FRTP04', 'FRTP05', 'FRTP06', ...
                'FRTP07', 'FRTP09', 'FRTP11', 'FRTP12', ...
                'FRTP14', 'FRTP16', 'FRTP17', 'FRTP18', 'FRTP20', 'FRTP21',  ...
                'FRTP22','FRTP23', 'FRTP24', 'FRTP25', 'FRTP26', 'FRTP27', 'FRTP28', ...
                'FRTP29', 'FRTP30', 'FRTP31', 'FRTP32', 'FRTP33'};
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\tvns_experiment\'; 
csv_file = 'C:\Users\denyer\OneDrive - UCL\FRT_data\tvns_experiment\all_pats_tvns_hit_data.csv'; 

kinarm_delay = 0.02; 

%% Load CSV Data for Phenotype 
fprintf('Loading behavioral CSV data...\n');
behavioral_df = readtable(csv_file, 'VariableNamingRule', 'preserve');

% Initialize master table to store 6 averaged traces per participant
subject_traces = table();
row_idx = 1;

for p = 1:length(participants)
    pat = participants{p};
    fprintf('Processing Subject: %s (%d of %d)\n', pat, p, length(participants));
    
    pat_dir = fullfile(base_dir, pat);
    compact_file = fullfile(pat_dir, [pat, '_tvns_reach_data_compact.mat']);
    
    if ~exist(compact_file, 'file')
        warning('Compact file missing for %s, skipping...', pat);
        continue;
    end
    
    %% 1. Extract Between-Subjects Factor: Responder Status from CSV
    idx_csv = strcmp(behavioral_df.SID, pat);
    if ~any(idx_csv)
        warning('Participant %s not found in CSV, skipping...', pat);
        continue;
    end
    
    % Grab the pos_responder value (1 = Positive, 0 = Negative)
    is_pos_responder = behavioral_df.pos_responder(find(idx_csv, 1));
    
    %% 2. Calculate Within-Subjects Factor: Trial-Level Baseline Split (Tertiles)
    L_comp = load(compact_file);
    tvns_data = L_comp.Compact_all;
    
    % Identify trials with clean pupil data
    valid_pupil = tvns_data.pupil_nan_percent < 0.3;
    
    % Extract trial-level baselines (mean of first 2000 samples of raw pupil)
    n_trials = height(tvns_data);
    trial_baselines = nan(n_trials, 1);
    
    for t = 1:n_trials
        if valid_pupil(t)
            trace = tvns_data.pupil_size{t};
            if ~isempty(trace) && ~all(isnan(trace))
                trial_baselines(t) = mean(trace(1:2000), 'omitnan');
            end
        end
    end
    
    % Determine THIS participant's tertile cutoffs
    valid_bases = trial_baselines(~isnan(trial_baselines));
    if isempty(valid_bases)
        continue; 
    end
    
    tertiles = prctile(valid_bases, [33.33, 66.67]);
    t1_cutoff = tertiles(1);
    t2_cutoff = tertiles(2);
    
    %% 3. Average Normalized Traces across the 6 Splits
    for stim = 1:2
        for base_level = 1:3
            if base_level == 1
                trial_idx = (tvns_data.stim_type == stim) & valid_pupil & (trial_baselines <= t1_cutoff);
            elseif base_level == 2
                trial_idx = (tvns_data.stim_type == stim) & valid_pupil & (trial_baselines > t1_cutoff) & (trial_baselines <= t2_cutoff);
            else
                trial_idx = (tvns_data.stim_type == stim) & valid_pupil & (trial_baselines > t2_cutoff);
            end
            
            cellArrays = tvns_data.pupil_size_normalized(trial_idx);
            nonEmptyCells = cellfun(@(x) x', cellArrays(~cellfun(@isempty, cellArrays) & ...
                                       ~cellfun(@(x) all(isnan(x)), cellArrays)), ...
                                       'UniformOutput', false);
            
            if ~isempty(nonEmptyCells)
                numericData = cell2mat(nonEmptyCells);
                
                trialMeans = mean(numericData, 2);
                outlierIdx = isoutlier(trialMeans);
                numericData = numericData(~outlierIdx, :);
                
                mean_trace = mean(numericData, 1);
                
                subject_traces.SID{row_idx} = pat;
                subject_traces.PosResponder(row_idx) = is_pos_responder;
                subject_traces.StimCondition(row_idx) = stim;
                subject_traces.BaseLevel(row_idx) = base_level; 
                subject_traces.NormTrace{row_idx} = mean_trace;
                
                row_idx = row_idx + 1;
            end
        end
    end
end

%% 4. Plotting the 1x2 Grid (Focus on Ramp Window)
fig = figure('Color', 'w', 'Position', [100, 100, 1200, 600]); 
t = tiledlayout(1, 2, 'TileSpacing', 'Compact', 'Padding', 'Compact');

% Time vector and Ramp Indices
fs = 1000;
time_seconds = ((0:12000) / fs) - 2;

idx_start = 4000; 
idx_end = 5500;
time_sub = time_seconds(idx_start:idx_end);

% Aesthetics
colors = {[0, 0.447, 0.741], [0.85, 0.325, 0.098]}; % Blue=Sham, Orange=tVNS
line_styles = {'-', '--', ':'}; % Low=Solid, Med=Dashed, High=Dotted
stim_names = {'Sham', 'tVNS'};
base_names = {'Low Base', 'Med Base', 'High Base'};

% 1=Positive Responders, 0=Negative Responders
resp_vals = [1, 0];
titles = {'Positive Responders', 'Negative Responders'};

for tile_idx = 1:2
    nexttile;
    hold on;
    
    curr_resp = resp_vals(tile_idx);
    
    % Loop through Stim then Baseline to build the 6 lines
    for stim = 1:2
        for base_level = 1:3
            
            % Filter for this specific line
            match_rows = subject_traces(subject_traces.PosResponder == curr_resp & ...
                                        subject_traces.StimCondition == stim & ...
                                        subject_traces.BaseLevel == base_level, :);
            
            if ~isempty(match_rows)
                norm_matrix = cell2mat(match_rows.NormTrace);
                
                % Subset data to just the ramp window
                norm_matrix_sub = norm_matrix(:, idx_start:idx_end);
                n_subs = size(norm_matrix_sub, 1);
                
                mu = mean(norm_matrix_sub, 1);
                sem = std(norm_matrix_sub, 0, 1) ./ sqrt(n_subs);
                
                % Plot SEM Ribbon (highly transparent to avoid clutter)
                fill([time_sub, fliplr(time_sub)], [(mu + sem), fliplr(mu - sem)], ...
                     colors{stim}, 'FaceAlpha', 0.05, 'EdgeColor', 'none', 'HandleVisibility', 'off');
                
                % Plot Mean Line
                disp_name = sprintf('%s | %s', stim_names{stim}, base_names{base_level});
                plot(time_sub, mu, line_styles{base_level}, 'Color', colors{stim}, ...
                     'LineWidth', 2.5, 'DisplayName', disp_name);
            end
        end
    end
    
    % Formatting
    title(titles{tile_idx}, 'FontSize', 12);
    grid on; xlabel('Time (seconds)');
    xlim([time_sub(1), time_sub(end)]);
    
    if tile_idx == 1; ylabel('Normalized Phasic Pupil Size'); end
    if tile_idx == 2; legend('Location', 'eastoutside'); end
end

title(t, 'Phasic Pupil Ramp Dynamics (4000:5500) by Phenotype', 'FontSize', 14, 'FontWeight', 'Bold');
linkaxes(findall(fig, 'type', 'axes'), 'y');