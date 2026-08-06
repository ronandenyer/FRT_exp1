%% Define Participant details
pat = 'FRTP33';
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\tvns_experiment'; 
pat_dir = strcat(base_dir, '\', pat);

tvns_file = strcat(pat_dir, '\', pat, '_tvns_reach_data_analyzed.mat');
load(tvns_file);
tvns_data = tvns_data_light;
kinarm_delay = 0.02; 

sham_exclude_blocks = [];
tvns_exclude_blocks = [];



tvns_plot_sliding_window_accuracy((tvns_data.eff_pt/1000)-kinarm_delay, tvns_data.success, tvns_data.stim_type);