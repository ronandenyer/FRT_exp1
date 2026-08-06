%% code to put all hit/prep time data in one data frame for reward ad tvns exps

%% reward
participants = {'FRTP01', 'FRTP02', 'FRTP03', 'FRTP04', 'FRTP05', 'FRTP06', 'FRTP07', ...
    'FRTP09', 'FRTP11', 'FRTP12', 'FRTP14', 'FRTP16', 'FRTP17', ...
    'FRTP18', 'FRTP20', 'FRTP21', 'FRTP22', 'FRTP23', 'FRTP24', 'FRTP25', 'FRTP26', ...
    'FRTP27', 'FRTP28', 'FRTP29', 'FRTP30', 'FRTP31', 'FRTP32', 'FRTP33'}; 
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\Reward_experiment';

all_reward_data = table();

for i = 1:length(participants)
    pat = participants{i};
    fprintf('Fitting Participant: `%s... ', pat);
    
    % 1. Load Data
    pat_file = fullfile(base_dir, pat, [pat '_reward_reach_data_analyzed.mat']);
    L = load(pat_file);
    data = L.reward_data;
    data = data(data.catch_trial == 1, :);
    
    data = data(:,{'SID', 'eff_pt', 'success', 'rt', 'motion_angle_rot', 'peak_tanVel', 'reward_type', 'prev_trial_success', 'end_target'});

    all_reward_data = vertcat(all_reward_data, data);

end

all_reward_data.reward_type = all_reward_data.reward_type - 2; % 1 = NO REWARD, 2 = REWARD

save_file_reward = 'all_pats_reward_hit_data.csv';
writetable(all_reward_data, strcat(base_dir, '\', save_file_reward));