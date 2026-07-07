%% Define Group Details
participants = {'FRTP01', 'FRTP02', 'FRTP03', 'FRTP04', 'FRTP05', 'FRTP06', 'FRTP07', ...
    'FRTP09', 'FRTP11', 'FRTP12', 'FRTP14', 'FRTP16', 'FRTP17', ...
    'FRTP18', 'FRTP20', 'FRTP21', 'FRTP22', 'FRTP23', 'FRTP24', 'FRTP25', 'FRTP26', ...
    'FRTP27', 'FRTP28', 'FRTP29'}; 
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\Reward_experiment'; 

all_angles_rad = []; % Initialize empty vector to store group data
all_reward_condition = [];
%% Loop through participants
for i = 1:length(participants)
    pat = participants{i};
    pat_dir = fullfile(base_dir, pat); % Using fullfile is safer for paths
    reward_file = fullfile(pat_dir, [pat, '_reward_reach_data_analyzed.mat']);
    
    % Check if file exists before loading to avoid crashes
    if exist(reward_file, 'file')
        load(reward_file);
        
        % Extract angles for catch_trial == 2
        % Ensure we are grabbing a column vector for consistent concatenation
        current_angles_deg = reward_data.motion_angle(reward_data.catch_trial == 2);
        reward_condition = reward_data.reward_type(reward_data.catch_trial == 2);
        disp(length(reward_condition(reward_condition == 3)))
        disp(length(reward_condition(reward_condition == 4)))

        all_angles_rad = [all_angles_rad; deg2rad(current_angles_deg(:))];
        all_reward_condition = [all_reward_condition; reward_condition(:)];
    else
        fprintf('Warning: File not found for participant %s\n', pat);
    end
end

%% Visualize Group Data in 3 Panels
figure('Color', 'w', 'Name', 'Group Reach Directions');

% Define common plotting parameters
bin_w = deg2rad(11.25);
blue_color = [0.2 0.5 0.8];

% --- Panel 1: All Catch Trials ---
subplot(1, 3, 1);
p1 = polarhistogram(all_angles_rad, 'BinWidth', bin_w);
p1.FaceColor = blue_color;
p1.EdgeColor = 'w';
title('All Catch Trials');

% --- Panel 2: No Reward (Condition 3) ---
subplot(1, 3, 2);
p2 = polarhistogram(all_angles_rad(all_reward_condition == 3), 'BinWidth', bin_w);
p2.FaceColor = [0.8 0.3 0.3]; % Reddish for 'No Reward'
p2.EdgeColor = 'w';
title('No Reward (C3)');

% --- Panel 3: Reward (Condition 4) ---
subplot(1, 3, 3);
p3 = polarhistogram(all_angles_rad(all_reward_condition == 4), 'BinWidth', bin_w);
p3.FaceColor = [0.2 0.7 0.3]; % Greenish for 'Reward'
p3.EdgeColor = 'w';
title('Reward (C4)');

%% Final Adjustments for Comparison
% To compare them fairly, they should all use the same radial scale
max_count = max([max(p1.Values), max(p2.Values), max(p3.Values)]);

for i = 1:3
    subplot(1, 3, i);
    rlim([0 max_count]); % Sets the same outer circle limit for all 3
end