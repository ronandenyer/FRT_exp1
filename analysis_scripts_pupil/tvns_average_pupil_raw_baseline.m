%% Script to get average pupil size for sham/tvns within a set time window
participants = {'FRTP01', 'FRTP02', 'FRTP03', 'FRTP04', 'FRTP05', 'FRTP06', ...
                'FRTP07', 'FRTP09', 'FRTP11', 'FRTP12', ...
                'FRTP14', 'FRTP16', 'FRTP17', 'FRTP18', 'FRTP20', 'FRTP21',  ...
                'FRTP22','FRTP23', 'FRTP24', 'FRTP25', 'FRTP26', 'FRTP27', 'FRTP28', ...
                'FRTP29', 'FRTP30', 'FRTP31', 'FRTP32', 'FRTP33'};
%% load data table

base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\tvns_experiment\'; 

%filename = 'pat_pupil_average_global_rest.mat';
filename = 'letswave_format_pupil_data.mat';
load(strcat(base_dir, filename));



%% get mean and max for tvns traces
fs = 1000; %sample rate pupil data
window_start = 0.001 * fs;
window_end = 2.001*fs;

tvns_baseline_average = table();

for i = 1:length(participants)

    tvns_baseline_average.SID(i) = participants(i);
    tvns_baseline_average.stim_type(i) = "tVNS";
    tvns_baseline_average.mean_pupil_size_sig_window(i) = mean(tvns_mat_raw(i,window_start:window_end));
    tvns_baseline_average.max_pupil_size_sig_window(i) = max(tvns_mat_raw(i,window_start:window_end));

end

%% get mean and max for sham traces

sham_sig_window_average = table();

for i = 1:length(participants)

    sham_sig_window_average.SID(i) = participants(i);
    sham_sig_window_average.stim_type(i) = "Sham";
    sham_sig_window_average.mean_pupil_size_sig_window(i) = mean(sham_mat_raw(i,window_start:window_end));
    sham_sig_window_average.max_pupil_size_sig_window(i) = max(sham_mat_raw(i,window_start:window_end));

end

%% combine to one table

sig_window_pupil_size_average = vertcat(tvns_baseline_average, sham_sig_window_average);

% save to csv
writetable(sig_window_pupil_size_average, strcat(base_dir, 'tvns_pupil_raw_means.csv'))