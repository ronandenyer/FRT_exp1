%% code to put all hit/prep time data in one data frame for reward and tvns exps
% Reverted to hardcoded columns with added Median Split (High/Low Sham Sigma)

participants = {'FRTP01', 'FRTP02', 'FRTP03', 'FRTP04', 'FRTP05', 'FRTP06', ...
                'FRTP07', 'FRTP09', 'FRTP11', 'FRTP12', ...
                'FRTP14', 'FRTP16', 'FRTP17', 'FRTP18', 'FRTP20', 'FRTP21',  ...
                'FRTP22','FRTP23', 'FRTP24', 'FRTP25', 'FRTP26', 'FRTP27', 'FRTP28', ...
                'FRTP29', 'FRTP30', 'FRTP31', 'FRTP32', 'FRTP33'};

base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\Reward_experiment'; 
kinarm_delay = 0.0; % Consistent with your fitting script



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
                   'pupil_size_phasic_mean_norm', 'pupil_size_pre_phasic_mean', ...
                   'pupil_size_pre_phasic_mean_norm', 'pupil_size_phasic_slope', ...
                   'pupil_size_max', 'pupil_size_max_ind', 'peak_tanVel', ...
                   'baseline_mean_force_z', 'post_cue_mean_force_z', 'post_cue_mean_force_z_norm', ...
                   'baseline_jitter', 'post_cue_jitter', 'force_jitter_normalized', 'post_cue_jitter_norm' ...
                   'prev_trial_success', 'reward_type', 'end_target'};
    data = data(:, target_cols); %
    % Inject Subject-Level Covariates and Group Splits
   

    all_reward_data = vertcat(all_reward_data, data); %
end

% --- STEP 3: SAVE TO CSV FOR R ---
save_file_reward = 'all_pats_reward_hit_data.csv'; %
export_path = fullfile(base_dir, save_file_reward);
writetable(all_reward_data, export_path); %
fprintf('\nSuccessfully saved master dataframe with covariates to: %s\n', export_path);