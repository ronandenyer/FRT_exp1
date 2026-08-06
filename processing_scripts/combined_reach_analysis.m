%% Define Participant details
%pat = 'FRTC05';
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\Combined_experiment'; 
pat_dir = strcat(base_dir, '\', pat);

reward_file = strcat(pat_dir, '\', pat, '_combined_reach_data_compact.mat');
load(reward_file);
combined_data = Compact_all;

%% step 1: for each trial, calculate reaction time
% Reaction time defined as time at which tan vel greater than 0.02 m/s

rt_threshold = 0.02;  % velocity threshold
firstAbove = nan(height(combined_data),1);  % store index of first exceedance

for i = 1:height(combined_data)

    vtan = combined_data.TanVel{i};
    % Find first index where velocity exceeds threshold
    idx = find(vtan > rt_threshold, 1, 'first');
    
    if ~isempty(idx)
        combined_data.rt(i) = idx;  % index = time in ms
    else
        combined_data.rt(i) = NaN;  % never exceeded
    end

end

% now calcualte the effective preptime based on established rt from data

combined_data.eff_pt = combined_data.rt - (500-combined_data.prep_time);

%% step 2: establish if the trial was an error or a success

angle_threshold = 45;
angle_threshold_upper = 90+angle_threshold;
angle_threshold_lower = 90-angle_threshold;
angle_index = 100; % time after movement onset we use to pick movement angle

for i = 1:height(combined_data)
    % establish the angle of motion 100 ms after movement onset
    if ~isnan(combined_data.rt(i))
        idx = combined_data.rt(i) + angle_index;
        combined_data.motion_angle(i) = combined_data.VelAngle{i}(idx);
        combined_data.motion_angle_rot(i) = combined_data.VelAngle_rot{i}(idx);
    
        if combined_data.motion_angle_rot(i) < angle_threshold_upper && combined_data.motion_angle_rot(i) > angle_threshold_lower
            combined_data.success(i) = 1;
        else
            combined_data.success(i) = 0;
        end
    else
        combined_data.motion_angle(i) = NaN;
        combined_data.motion_angle_rot(i) = NaN;
        combined_data.success(i) = NaN;
    end

end

%% step 3: get the peak velocity of each reach trial

for i = 1:height(combined_data)

    combined_data.peak_tanVel(i) = max(combined_data.TanVel{i}(1:800));


end

%% step 4: get previous trial hit/miss
combined_data.prev_trial_success(1) = NaN;
for i = 2:height(combined_data)
    
    if combined_data.trial_num(i) ~= 1 && combined_data.catch_trial(i-1) ~= 2
        combined_data.prev_trial_success(i) = combined_data.feedback_success(i-1);
    else
        combined_data.prev_trial_success(i) = NaN;
    end
end
%% Save data
combined_data = combined_data(:, [1:9, 14:17, 23:42]);

save_filename = strcat(pat, '_combined_reach_data_analyzed.mat');
save_path = strcat(pat_dir, '\', save_filename);
save(save_path, 'combined_data');