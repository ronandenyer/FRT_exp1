%% code to put all hit/prep time data in one data frame for reward and tvns exps
% Reverted to hardcoded columns with added Median Split (High/Low Sham Sigma)

participants = {'FRTC01', 'FRTC02', 'FRTC03', 'FRTC04', 'FRTC05'};

base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\Combined_experiment'; 



%% --- STEP 2: COMPILE SINGLE-TRIAL DATA & INJECT COVARIATES ---
fprintf('\nCompiling single-trial dataframe...\n');
all_combined_data = table();

for i = 1:length(participants)
    pat = participants{i};
    fprintf('Participant: %s... ', pat);
    
    % Load Data
    pat_file = fullfile(base_dir, pat, [pat '_combined_reach_data_analyzed.mat']);
    if ~exist(pat_file, 'file')
        fprintf('File not found.\n');
        continue; 
    end
    L = load(pat_file);
    data = L.combined_data;
    
    % Select target columns (Your original hardcoded list)
    target_cols = {'SID', 'block_num', 'trial_num', 'eff_pt', 'success', 'rt', ...
                   'motion_angle_rot', 'pupil_size_mean', 'pupil_size_phasic_mean', ...
                   'pupil_size_phasic_mean_norm', 'pupil_size_phasic_slope','pupil_size_pre_phasic_mean', 'pupil_size_pre_phasic_mean_norm', 'pupil_size_max', 'peak_tanVel', ...
                   'stim_type', 'reward_type', 'prev_trial_success', 'end_target'};
    data = data(:, target_cols); 
    

    all_combined_data = vertcat(all_combined_data, data);
end
% --- STEP 3: SAVE TO CSV FOR R ---
save_file_tvns = 'all_pats_combined_hit_data.csv'; 
export_path = fullfile(base_dir, save_file_tvns);
writetable(all_combined_data, export_path); 
fprintf('\nSuccessfully saved master dataframe with covariates to: %s\n', export_path);