%% Define Participant details
%% Define Participant details
pat = 'FRTP15';
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\Reward_experiment'; 
pat_dir = strcat(base_dir, '\', pat);

reward_file = strcat(pat_dir, '\', pat, '_reward_reach_data_analyzed.mat');
load(reward_file);
exclude_blocks = [];
kinarm_delay = 0.02; %KINARM system delay in seconds



reward_plot_sliding_window_accuracy((reward_data.eff_pt/1000)-kinarm_delay, reward_data.success, reward_data.reward_type);