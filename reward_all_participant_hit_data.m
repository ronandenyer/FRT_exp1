%% code to put all hit/prep time data in one data frame for reward and tvns exps
% Reverted to hardcoded columns with added Median Split (High/Low Sham Sigma)

participants = {'FRTP01', 'FRTP02', 'FRTP03', 'FRTP04', 'FRTP05', 'FRTP06', ...
                'FRTP07', 'FRTP09', 'FRTP11', 'FRTP12', ...
                'FRTP14', 'FRTP16', 'FRTP17', 'FRTP18', 'FRTP20', 'FRTP21',  ...
                'FRTP22','FRTP23', 'FRTP24', 'FRTP25', 'FRTP26', 'FRTP27', 'FRTP28', ...
                'FRTP29', 'FRTP30', 'FRTP31', 'FRTP32', 'FRTP33'};

base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\Reward_experiment'; 
kinarm_delay = 0.0; % Consistent with your fitting script

%% --- STEP 1: COMPUTE SUBJECT-LEVEL COVARIATES ---
fprintf('Pre-computing SAT model fits for covariates...\n');

covariatesMap_no_reward = containers.Map('KeyType', 'char', 'ValueType', 'double');
covariatesMap_Change = containers.Map('KeyType', 'char', 'ValueType', 'double');

for p = 1:length(participants)
    pat = participants{p};
    pat_file = fullfile(base_dir, pat, [pat, '_reward_reach_data_analyzed.mat']);
    if ~exist(pat_file, 'file'); continue; end
    
    L = load(pat_file);
    data_fit = L.reward_data; 
    
    NO_REWARD = 3; REWARD = 4;                
    valid_trials = (~isnan(data_fit.eff_pt) & data_fit.catch_trial == 1);                
    data_fit = data_fit(valid_trials, :);
    
    try
        % Fit Sham
        idx_no_reward = (data_fit.reward_type == NO_REWARD);
        no_reward_model = fit_model((data_fit.eff_pt(idx_no_reward) / 1000) - kinarm_delay, data_fit.success(idx_no_reward));
        no_reward_sigma = no_reward_model.paramsOpt(1); %

        % Fit Active
        idx_reward = (data_fit.reward_type == REWARD);
        reward_model = fit_model((data_fit.eff_pt(idx_reward) / 1000) - kinarm_delay, data_fit.success(idx_reward));
        reward_sigma = reward_model.paramsOpt(1);

        % Calculate Change Score
        change_sigma = reward_sigma - no_reward_sigma; %

        % Store values if fits didn't resolve to NaN
        if ~isnan(no_reward_sigma) && ~isnan(reward_sigma)
            covariatesMap_no_reward(pat) = no_reward_sigma;
            covariatesMap_Change(pat) = change_sigma;
        end
    catch ME
        fprintf('Warning: Failed to fit model for %s. Error: %s\n', pat, ME.message);
    end
end

%% --- STEP 1b: COMPUTE MEDIAN SPLIT FOR SHAM SIGMA ---
fprintf('Calculating median split groups for Sham Sigma...\n');

covariatesMap_Group = containers.Map('KeyType', 'char', 'ValueType', 'double');

if ~isempty(covariatesMap_no_reward)
    % Extract all valid sham sigma values to find the actual group median
    all_sigmas = cell2mat(values(covariatesMap_no_reward));
    median_no_reward_sigma = median(all_sigmas); %
    fprintf('Group Median Sham Sigma = %.4f\n', median_no_reward_sigma);
    
    % Assign 1 for High (> Median) and 0 for Low (<= Median)
    keys_list = keys(covariatesMap_no_reward);
    for k = 1:length(keys_list)
        pat_key = keys_list{k};
        if covariatesMap_no_reward(pat_key) > median_no_reward_sigma
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
all_reward_data = table();

for i = 1:length(participants)
    pat = participants{i};
    fprintf('Participant: %s... ', pat);
    
    % Load Data
    pat_file = fullfile(base_dir, pat, [pat '_reward_reach_data_analyzed.mat']);
    if ~exist(pat_file, 'file')
        fprintf('File not found.\n');
        continue; 
    end
    L = load(pat_file);
    data = L.reward_data;
    
    % Select target columns (Your original hardcoded list)
    target_cols = {'SID', 'block_num', 'trial_num', 'eff_pt', 'success', 'rt', ...
                   'motion_angle_rot', 'pupil_size_mean', 'pupil_size_phasic_mean', ...
                   'pupil_size_phasic_mean_norm', 'pupil_size_phasic_slope', 'peak_tanVel', ...
                   'reward_type', 'prev_trial_success', 'end_target'};
    data = data(:, target_cols); %
    % Inject Subject-Level Covariates and Group Splits
    if isKey(covariatesMap_no_reward, pat)
        data.NoReward_sigma = repmat(covariatesMap_no_reward(pat), height(data), 1);
        data.ChangeSigma = repmat(covariatesMap_Change(pat), height(data), 1);
        data.HighNoRewardSigma = repmat(covariatesMap_Group(pat), height(data), 1);
        fprintf('Done (with covariates & median split).\n');
    else
        % If fitting failed for this subject, fill with NaN
        data.no_reward_sigma = nan(height(data), 1);
        data.ChangeSigma = nan(height(data), 1);
        data.HighNoRewardSigma = nan(height(data), 1);
        fprintf('Done (No fits available, assigned NaN).\n');
    end

    all_reward_data = vertcat(all_reward_data, data); %
end

% --- STEP 3: SAVE TO CSV FOR R ---
save_file_reward = 'all_pats_reward_hit_data.csv'; %
export_path = fullfile(base_dir, save_file_reward);
writetable(all_reward_data, export_path); %
fprintf('\nSuccessfully saved master dataframe with covariates to: %s\n', export_path);