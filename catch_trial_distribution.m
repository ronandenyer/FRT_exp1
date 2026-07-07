%% Define Participant details
pat = 'FRTP04';
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\Reward_experiment'; 
pat_dir = strcat(base_dir, '\', pat);

reward_file = strcat(pat_dir, '\', pat, '_reward_reach_data_analyzed.mat');
load(reward_file);
exclude_blocks = [];

%%
%histogram(reward_data.motion_angle(reward_data.catch_trial == 2), "BinWidth", 1, "BinLimits", [-180 180]);

% 1. Extract your data
angles_deg = reward_data.motion_angle(reward_data.catch_trial == 2);

% 2. Convert degrees to radians
angles_rad = deg2rad(angles_deg);

% 3. Create the polar histogram
polarhistogram(angles_rad, 'BinWidth', deg2rad(10)); % Adjust BinWidth as needed