%% code to put all hit/prep time data in one data frame for reward and tvns exps
% Reverted to hardcoded columns with added Median Split (High/Low Sham Sigma)

participants = {'FRTP01', 'FRTP02', 'FRTP03', 'FRTP04', 'FRTP05', 'FRTP06', ...
                'FRTP07', 'FRTP09', 'FRTP11', 'FRTP12', ...
                'FRTP14', 'FRTP16', 'FRTP17', 'FRTP18', 'FRTP20', 'FRTP21',  ...
                'FRTP22','FRTP23', 'FRTP24', 'FRTP25', 'FRTP26', 'FRTP27', 'FRTP28', ...
                'FRTP29', 'FRTP30', 'FRTP31', 'FRTP32', 'FRTP33'};

base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\tVNS_experiment'; 
kinarm_delay = 0.0; % Consistent with your fitting script[cite: 1]

%% --- STEP 1: COMPUTE SUBJECT-LEVEL COVARIATES ---
fprintf('Pre-computing SAT model fits for covariates...\n');

covariatesMap_Sham = containers.Map('KeyType', 'char', 'ValueType', 'double');
covariatesMap_Change = containers.Map('KeyType', 'char', 'ValueType', 'double');

for p = 1:length(participants)
    pat = participants{p};
    pat_file = fullfile(base_dir, pat, [pat, '_tvns_reach_data_analyzed.mat']);
    if ~exist(pat_file, 'file'); continue; end
    
    L = load(pat_file);
    data_fit = L.tvns_data_light; 
    
    SHAM = 1; ACTIVE = 2;                
    valid_trials = (~isnan(data_fit.eff_pt) & data_fit.catch_trial == 1);                
    data_fit = data_fit(valid_trials, :);
    
    try
        % Fit Sham
        idx_sham = (data_fit.stim_type == SHAM);
        sham_model = fit_model_fixed_sigma((data_fit.eff_pt(idx_sham) / 1000) - kinarm_delay, data_fit.success(idx_sham));
        sham_sigma = sham_model.paramsOpt(1); %[cite: 1]

        % Fit Active
        idx_active = (data_fit.stim_type == ACTIVE);
        active_model = fit_model_fixed_sigma((data_fit.eff_pt(idx_active) / 1000) - kinarm_delay, data_fit.success(idx_active));
        active_sigma = active_model.paramsOpt(1);

        % Calculate Change Score
        change_sigma = active_sigma - sham_sigma; %[cite: 1]

        % Store values if fits didn't resolve to NaN
        if ~isnan(sham_sigma) && ~isnan(active_sigma)
            covariatesMap_Sham(pat) = sham_sigma;
            covariatesMap_Change(pat) = change_sigma;
        end
    catch ME
        fprintf('Warning: Failed to fit model for %s. Error: %s\n', pat, ME.message);
    end
end

%% --- STEP 1b: COMPUTE MEDIAN SPLIT FOR SHAM SIGMA ---
fprintf('Calculating median split groups for Sham Sigma...\n');

covariatesMap_Group = containers.Map('KeyType', 'char', 'ValueType', 'double');

if ~isempty(covariatesMap_Sham)
    % Extract all valid sham sigma values to find the actual group median[cite: 1]
    all_sigmas = cell2mat(values(covariatesMap_Sham));
    median_sham_sigma = median(all_sigmas); %[cite: 1]
    fprintf('Group Median Sham Sigma = %.4f\n', median_sham_sigma);
    
    % Assign 1 for High (> Median) and 0 for Low (<= Median)[cite: 1]
    keys_list = keys(covariatesMap_Sham);
    for k = 1:length(keys_list)
        pat_key = keys_list{k};
        if covariatesMap_Sham(pat_key) > median_sham_sigma
            covariatesMap_Group(pat_key) = 1; % High Sham Sigma Group
        else
            covariatesMap_Group(pat_key) = 0; % Low Sham Sigma Group
        end
    end
else
    error('No valid model fits were calculated. Cannot compute median split.');
end

%% --- STEP 2: COMPILE SINGLE-TRIAL DATA & INJECT COVARIATES ---
fprintf('\nCompiling single-trial dataframe...\n');
all_tvns_data = table();

for i = 1:length(participants)
    pat = participants{i};
    fprintf('Participant: %s... ', pat);
    
    % Load Data
    pat_file = fullfile(base_dir, pat, [pat '_tvns_reach_data_analyzed.mat']);
    if ~exist(pat_file, 'file')
        fprintf('File not found.\n');
        continue; 
    end
    L = load(pat_file);
    data = L.tvns_data_light;
    
    % Select target columns (Your original hardcoded list)[cite: 2]
    target_cols = {'SID', 'block_num', 'trial_num', 'eff_pt', 'success', 'rt', ...
                   'motion_angle_rot', 'pupil_size_mean', 'pupil_size_phasic_mean', ...
                   'pupil_size_phasic_mean_norm', 'pupil_size_phasic_slope', 'pupil_size_max', 'peak_tanVel', ...
                   'stim_type', 'prev_trial_success', 'end_target'};
    data = data(:, target_cols); %[cite: 2]
    % Inject Subject-Level Covariates and Group Splits
    if isKey(covariatesMap_Sham, pat)
        data.Sham_sigma = repmat(covariatesMap_Sham(pat), height(data), 1);
        data.ChangeSigma = repmat(covariatesMap_Change(pat), height(data), 1);
        data.HighShamSigma = repmat(covariatesMap_Group(pat), height(data), 1);
        fprintf('Done (with covariates & median split).\n');
    else
        % If fitting failed for this subject, fill with NaN
        data.Sham_sigma = nan(height(data), 1);
        data.ChangeSigma = nan(height(data), 1);
        data.HighShamSigma = nan(height(data), 1);
        fprintf('Done (No fits available, assigned NaN).\n');
    end

    all_tvns_data = vertcat(all_tvns_data, data); %[cite: 2]
end

% --- STEP 3: SAVE TO CSV FOR R ---
save_file_tvns = 'all_pats_tvns_hit_data.csv'; %[cite: 2]
export_path = fullfile(base_dir, save_file_tvns);
writetable(all_tvns_data, export_path); %[cite: 2]
fprintf('\nSuccessfully saved master dataframe with covariates to: %s\n', export_path);