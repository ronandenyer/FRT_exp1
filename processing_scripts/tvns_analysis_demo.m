%% Script to visualize various steps of data processing of FRT reach analysis
%% load data
pat = 'LEA';
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\tVNS_experiment'; 
pat_dir = strcat(base_dir, '\', pat);

tvns_file = strcat(pat_dir, '\', pat, '_tvns_reach_data_compact.mat');
load(tvns_file);
tvns_data = Compact_all;

%% Part 1: Analyzing WHEN movement was initiated

% Reaction time defined as time at which tan vel greater than 0.02 m/s
trial = 9;
rt_threshold = 0.02;  % velocity threshold

vtan = tvns_data.TanVel{trial};

plot(vtan);
%% find where vtan first went went over 0.02
idx = find(vtan > rt_threshold, 1, 'first');

if ~isempty(idx)
        rt = idx;  % index = time in ms
else
        rt = NaN;  % never exceeded (no reach occurred)
end
disp("time where tangental velocity exceeded threshold: ")
disp(idx);


%% establish when target was displayed

target_time = tvns_data.prep_time_event_code(trial);
disp("Target displayed at: ")
disp(target_time);
%% establish effective preparation time
effective_prep_time = rt - target_time;
disp('Effective preparation time on this trial = ')
disp(effective_prep_time);

%% plot this data
hold on
plot(vtan);
xline(target_time, 'r--');
xline(idx, 'g--');

%% Part 2: Analyzing WHERE the movement was aimed

angle_threshold = 45;
angle_threshold_upper = 90+angle_threshold;
angle_threshold_lower = 90-angle_threshold;

angle_index = 100; % time after movement onset we use to pick movement angle

idx2 = rt + angle_index; % add on the index to the rt calcualted in previous step
disp(idx2);

%% plot this data
hold on
plot(vtan);
xline(target_time, 'r--');
xline(idx, 'g--');
xline(idx2, 'b--')
%%

VelAngle = tvns_data.VelAngle_rot{trial};
motion_angle_rot = VelAngle(idx2);

disp("initial reach angle: ")
disp(motion_angle_rot);
%% plot this data
hold on
plot(VelAngle);
xline(idx2, 'b--')

%%
if motion_angle_rot < angle_threshold_upper && motion_angle_rot > angle_threshold_lower
    success = 1;
else
    success = 0;
end

disp("Reach success result (1=hit/0=miss): ")
disp(success);

%% Part 3: Establish peak velocity
range_lower = 1;
range_upper = 800;

[peakVel, peakVel_time] = max(vtan(range_lower:range_upper));

disp("Peak velocity = ");
disp(peakVel);
disp("peak velocity time = ");
disp(peakVel_time);

%% plot this data
hold on
plot(vtan);
xline(peakVel_time, 'r--')

%% save key variables in a table

key_results = table();

key_results.pt(1) = effective_prep_time;
key_results.success(1) = success;
key_results.peakVel(1) = peakVel;