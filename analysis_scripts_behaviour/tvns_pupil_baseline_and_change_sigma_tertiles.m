%% Define Directory and Participants
participants = {'FRTP01', 'FRTP02', 'FRTP03', 'FRTP04', 'FRTP05', 'FRTP06', ...
                'FRTP07', 'FRTP09', 'FRTP11', 'FRTP12', ...
                'FRTP14', 'FRTP16', 'FRTP17', 'FRTP18', 'FRTP20', 'FRTP21',  ...
                'FRTP22','FRTP23', 'FRTP24', 'FRTP25', 'FRTP26', 'FRTP27', 'FRTP28', ...
                'FRTP29', 'FRTP30', 'FRTP31', 'FRTP32', 'FRTP33'};

base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\tvns_experiment\'; 
kinarm_delay = 0.0; 

% Initialize master table to store 6 averaged traces per participant (2 stim x 3 base groups)
subject_traces = table();
row_idx = 1;

for p = 1:length(participants)
    pat = participants{p};
    fprintf('Processing Subject: %s (%d of %d)\n', pat, p, length(participants));
    
    pat_dir = fullfile(base_dir, pat);
    analyzed_file = fullfile(pat_dir, [pat, '_tvns_reach_data_analyzed.mat']);
    compact_file = fullfile(pat_dir, [pat, '_tvns_reach_data_compact.mat']);
    
    if ~exist(analyzed_file, 'file') || ~exist(compact_file, 'file')
        warning('Files missing for %s, skipping...', pat);
        continue;
    end

    %% 1. Calculate Between-Subjects Factor: Change in Sigma
    L_ana = load(analyzed_file);
    data_fit = L_ana.tvns_data_light;
    
    valid_behavior = (~isnan(data_fit.eff_pt) & data_fit.catch_trial == 1);
    data_fit = data_fit(valid_behavior, :);
    
    try
        % Fit Sham
        idx_sham = (data_fit.stim_type == 1);
        sham_model = fit_model((data_fit.eff_pt(idx_sham) / 1000) - kinarm_delay, data_fit.success(idx_sham));
        sham_sigma = sham_model.paramsOpt(2);

        % Fit tVNS
        idx_active = (data_fit.stim_type == 2);
        active_model = fit_model((data_fit.eff_pt(idx_active) / 1000) - kinarm_delay, data_fit.success(idx_active));
        active_sigma = active_model.paramsOpt(2);

        change_sigma = active_sigma - sham_sigma;
    catch
        fprintf('Model fit failed for %s, skipping.\n', pat);
        continue;
    end
    
    if isnan(change_sigma); continue; end

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
    % Conditions: 1=Sham, 2=tVNS | Base: 1=Low, 2=Medium, 3=High
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
            cellArrays = tvns_data.pupil_size_normalized(trial_idx);
            nonEmptyCells = cellfun(@(x) x', cellArrays(~cellfun(@isempty, cellArrays) & ...
                                       ~cellfun(@(x) all(isnan(x)), cellArrays)), ...
                                       'UniformOutput', false);
            
            if ~isempty(nonEmptyCells)
                numericData = cell2mat(nonEmptyCells);
                
                % Outlier removal
                trialMeans = mean(numericData, 2);
                outlierIdx = isoutlier(trialMeans);
                numericData = numericData(~outlierIdx, :);
                
                % Grand average for this specific split
                mean_trace = mean(numericData, 1);
                
                % Save to subject_traces table
                subject_traces.SID{row_idx} = pat;
                subject_traces.ChangeSigma(row_idx) = change_sigma;
                subject_traces.StimCondition(row_idx) = stim;
                subject_traces.BaseLevel(row_idx) = base_level; % 1=Low, 2=Med, 3=High
                subject_traces.NormTrace{row_idx} = mean_trace;
                
                row_idx = row_idx + 1;
            end
        end
    end
end

%% 4. Calculate Between-Subjects Behavioral Tertiles (Change in Sigma)
% Extract one ChangeSigma value per subject to calculate true between-subject tertiles
unique_subs = unique(subject_traces.SID);
sub_sigmas = zeros(length(unique_subs), 1);

for s = 1:length(unique_subs)
    idx = find(strcmp(subject_traces.SID, unique_subs{s}), 1);
    sub_sigmas(s) = subject_traces.ChangeSigma(idx);
end

sigma_tertiles = prctile(sub_sigmas, [33.33, 66.67]);

% Assign a SigmaGroup to each row in the master table
subject_traces.SigmaGroup = zeros(height(subject_traces), 1);
subject_traces.SigmaGroup(subject_traces.ChangeSigma <= sigma_tertiles(1)) = 1; % Improvers
subject_traces.SigmaGroup(subject_traces.ChangeSigma > sigma_tertiles(1) & subject_traces.ChangeSigma <= sigma_tertiles(2)) = 2; % Non-Responders
subject_traces.SigmaGroup(subject_traces.ChangeSigma > sigma_tertiles(2)) = 3; % Worseners

%% 5. Plotting the 3x3 Grid
fig = figure('Color', 'w', 'Position', [100, 100, 1500, 1200]); % Made taller for 3 rows
t = tiledlayout(3, 3, 'TileSpacing', 'Compact', 'Padding', 'Compact');

fs = 1000;
time_seconds = ((0:12000) / fs) - 2;
colors = {[0, 0.447, 0.741], [0.85, 0.325, 0.098]}; % Blue (Sham), Orange (tVNS)
events = [2000, 4000, 5500, 6000];

row_titles = {'Improvers (Bottom 33% \Delta\sigma)', ...
              'Non-Responders (Middle 33% \Delta\sigma)', ...
              'Worseners (Top 33% \Delta\sigma)'};
col_titles = {'Low Baseline', 'Med Baseline', 'High Baseline'};

for sig_group = 1:3
    for base_level = 1:3
        nexttile;
        hold on;
        
        % Filter master table for this specific tile
        match_rows = subject_traces(subject_traces.SigmaGroup == sig_group & subject_traces.BaseLevel == base_level, :);
        n_pats = length(unique(match_rows.SID));
        
        for stim = 1:2
            % Extract traces for this condition
            stim_rows = match_rows(match_rows.StimCondition == stim, :);
            
            if ~isempty(stim_rows)
                norm_matrix = cell2mat(stim_rows.NormTrace);
                n_subs = size(norm_matrix, 1);
                
                mu = mean(norm_matrix, 1);
                sem = std(norm_matrix, 0, 1) ./ sqrt(n_subs);
                
                % Plot SEM Ribbon
                fill([time_seconds, fliplr(time_seconds)], [(mu + sem), fliplr(mu - sem)], ...
                     colors{stim}, 'FaceAlpha', 0.15, 'EdgeColor', 'none', 'HandleVisibility', 'off');
                
                % Plot Mean Line
                lineStyle = '-'; if stim == 2, lineStyle = '--'; end
                cond_name = 'Sham'; if stim == 2; cond_name = 'tVNS'; end
                
                plot(time_seconds, mu, lineStyle, 'Color', colors{stim}, 'LineWidth', 2, 'DisplayName', cond_name);
            end
        end
        
        % Formatting
        title(sprintf('%s | %s\n(n=%d)', row_titles{sig_group}, col_titles{base_level}, n_pats), 'FontSize', 11);
        grid on; 
        
        % Clean up axis labels and legends to prevent clutter
        if sig_group == 3; xlabel('Time (seconds)'); end % X-labels only on bottom row
        if base_level == 1; ylabel('Norm. Pupil Size'); end % Y-labels only on left column
        if sig_group == 1 && base_level == 3; legend('Location', 'northeast'); end % One legend top right
        
        % Event Lines
        yl = ylim;
        for e = 1:length(events)
            event_time = time_seconds(events(e));
            line([event_time event_time], yl, 'Color', [0.5 0.5 0.5], 'LineStyle', ':', 'HandleVisibility', 'off');
        end
    end
end

title(t, '3x3 Split: Behavioral Phenotypes x Trial-Level Baseline Arousal', 'FontSize', 16, 'FontWeight', 'Bold');

% Lock Y-axes across all subplots dynamically
linkaxes(findall(fig, 'type', 'axes'), 'y');