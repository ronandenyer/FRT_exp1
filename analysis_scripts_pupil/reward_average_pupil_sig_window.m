%% Script to get average pupil size for sham/tvns within a set time window
participants = {'FRTP01', 'FRTP02', 'FRTP03', 'FRTP04', 'FRTP05', 'FRTP06', ...
                'FRTP07', 'FRTP09', 'FRTP11', 'FRTP12', ...
                'FRTP14', 'FRTP16', 'FRTP17', 'FRTP18', 'FRTP20', 'FRTP21',  ...
                'FRTP22','FRTP23', 'FRTP24', 'FRTP25', 'FRTP26', 'FRTP27', 'FRTP28', ...
                'FRTP29', 'FRTP30', 'FRTP31', 'FRTP32', 'FRTP33'};
%% load data table

base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\Reward_experiment\'; 

%filename = 'pat_pupil_average_global_rest.mat';
filename = 'letswave_format_pupil_data.mat';
load(strcat(base_dir, filename));


%% load RMT data

%% get mean and max for tvns traces
fs = 1000; %sample rate pupil data
window_start = 4 * fs;
window_end = 5.5*fs;

reward_sig_window_average = table();

for i = 1:length(participants)

    reward_sig_window_average.SID(i) = participants(i);
    reward_sig_window_average.reward_type(i) = "reward";
    reward_sig_window_average.mean_pupil_size_sig_window(i) = mean(reward_mat(i,window_start:window_end));
    reward_sig_window_average.max_pupil_size_sig_window(i) = max(reward_mat(i,window_start:window_end));

end

%% get mean and max for sham traces

no_reward_sig_window_average = table();

for i = 1:length(participants)

    no_reward_sig_window_average.SID(i) = participants(i);
    no_reward_sig_window_average.reward_type(i) = "no_reward";
    no_reward_sig_window_average.mean_pupil_size_sig_window(i) = mean(no_reward_mat(i,window_start:window_end));
    no_reward_sig_window_average.max_pupil_size_sig_window(i) = max(no_reward_mat(i,window_start:window_end));

end

%% combine to one table

sig_window_pupil_size_average = vertcat(reward_sig_window_average, no_reward_sig_window_average);

% save to csv
writetable(sig_window_pupil_size_average, strcat(base_dir, 'reward_pupil_means.csv'))