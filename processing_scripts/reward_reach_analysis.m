%% Define Participant details
%pat = 'FRTP01';
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\Reward_experiment'; 
pat_dir = strcat(base_dir, '\', pat);

reward_file = strcat(pat_dir, '\', pat, '_reward_reach_data_compact.mat');
load(reward_file);
reward_data = Compact_all;

%% step 1: for each trial, calculate reaction time
% Reaction time defined as time at which tan vel greater than 0.02 m/s

rt_threshold = 0.02;  % velocity threshold
firstAbove = nan(height(reward_data),1);  % store index of first exceedance

for i = 1:height(reward_data)

    vtan = reward_data.TanVel{i};
    % Find first index where velocity exceeds threshold
    idx = find(vtan > rt_threshold, 1, 'first');
    
    if ~isempty(idx)
        reward_data.rt(i) = idx;  % index = time in ms
    else
        reward_data.rt(i) = NaN;  % never exceeded
    end

end

% now calcualte the effective preptime based on established rt from data

reward_data.eff_pt = reward_data.rt - (500-reward_data.prep_time);

%% step 2: establish if the trial was an error or a success

angle_threshold = 45;
angle_threshold_upper = 90+angle_threshold;
angle_threshold_lower = 90-angle_threshold;
angle_index = 100; % time after movement onset we use to pick movement angle

for i = 1:height(reward_data)
    % establish the angle of motion 100 ms after movement onset
    idx = reward_data.rt(i) + angle_index;
    reward_data.motion_angle(i) = reward_data.VelAngle{i}(idx);
    reward_data.motion_angle_rot(i) = reward_data.VelAngle_rot{i}(idx);

    if reward_data.motion_angle_rot(i) < angle_threshold_upper && reward_data.motion_angle_rot(i) > angle_threshold_lower
        reward_data.success(i) = 1;
    else
        reward_data.success(i) = 0;
    end

end

%% step 3: get the peak velocity of each reach trial

for i = 1:height(reward_data)

    reward_data.peak_tanVel(i) = max(reward_data.TanVel{i}(1:800));


end

%% step 4: get previous trial hit/miss
reward_data.prev_trial_success(1) = NaN;
for i = 2:height(reward_data)
    
    if reward_data.trial_num(i) ~= 1 && reward_data.catch_trial(i-1) ~= 2
        reward_data.prev_trial_success(i) = reward_data.feedback_success(i-1);
    else
        reward_data.prev_trial_success(i) = NaN;
    end
end
%% Save data
reward_data = reward_data(:, [1:7, 14:17, 23:50]);

save_filename = strcat(pat, '_reward_reach_data_analyzed.mat');
save_path = strcat(pat_dir, '\', save_filename);
save(save_path, 'reward_data');