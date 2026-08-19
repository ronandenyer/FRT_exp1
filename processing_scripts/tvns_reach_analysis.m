%% Define Participant details
%pat = 'FRTP33';
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\tVNS_experiment'; 
pat_dir = strcat(base_dir, '\', pat);

tvns_file = strcat(pat_dir, '\', pat, '_tvns_reach_data_compact.mat');
load(tvns_file);
tvns_data = Compact_all;

%% step 1: for each trial, calculate reaction time
% Reaction time defined as time at which tan vel greater than 0.02 m/s

rt_threshold = 0.02;  % velocity threshold
firstAbove = nan(height(tvns_data),1);  % store index of first exceedance

for i = 1:height(tvns_data)
    
    vtan = tvns_data.TanVel{i};
    % Find first index where velocity exceeds threshold
    idx = find(vtan > rt_threshold, 1, 'first');
    
    if ~isempty(idx)
        tvns_data.rt(i) = idx;  % index = time in ms
    else
        tvns_data.rt(i) = NaN;  % never exceeded
    end

end

% now calcualte the effective preptime based on established rt from data

tvns_data.eff_pt = tvns_data.rt - (500-tvns_data.prep_time);

%% step 2: establish if the trial was an error or a success

angle_threshold = 45;
angle_threshold_upper = 90+angle_threshold;
angle_threshold_lower = 90-angle_threshold;
angle_index = 100; % time after movement onset we use to pick movement angle

for i = 1:height(tvns_data)
    disp(i);
    % establish the angle of motion 100 ms after movement onset
    
    idx = tvns_data.rt(i) + angle_index;

    if ~isnan(idx)
        tvns_data.motion_angle(i) = tvns_data.VelAngle{i}(idx);
        tvns_data.motion_angle_rot(i) = tvns_data.VelAngle_rot{i}(idx);
    
        if tvns_data.motion_angle_rot(i) < angle_threshold_upper && tvns_data.motion_angle_rot(i) > angle_threshold_lower
            tvns_data.success(i) = 1;
        else
            tvns_data.success(i) = 0;
        end
    else
        tvns_data.success(i) = NaN;
    end

end

%% step 3: get the peak velocity of each reach trial

for i = 1:height(tvns_data)

    tvns_data.peak_tanVel(i) = max(tvns_data.TanVel{i}(250:750));


end

%% step 4: get previous trial hit/miss
tvns_data.prev_trial_success(1) = NaN;
for i = 2:height(tvns_data)
    
    if tvns_data.trial_num(i) ~= 1 && tvns_data.catch_trial(i-1) ~= 2
        tvns_data.prev_trial_success(i) = tvns_data.feedback_success(i-1);
    else
        tvns_data.prev_trial_success(i) = NaN;
    end
end

%% Strip out data heavy columns before saving
tvns_data_light = tvns_data(:, [1:7, 14:17, 23:50]);
%% Save data

save_filename = strcat(pat, '_tvns_reach_data_analyzed.mat');
save_path = strcat(pat_dir, '\', save_filename);
save(save_path, 'tvns_data_light');