%% Script to format pupil data in expected format for letswave

%% load data table

base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\Reward_experiment\'; 

%filename = 'pat_pupil_average_global_rest.mat';
filename = 'letswave_format_pupil_data.mat';
load(strcat(base_dir, filename));



%%
LW_init();
option.dimension_descriptors={'epochs','X','channels','Y'};
option.unit='amplitude';
option.xunit='time';
option.yunit='frequency';
option.xstart=-2;
option.xstep=0.001;
option.ystart=0;
option.ystep=1;
option.is_save=1;

option.filename='no_reward_mat';
FLW_import_mat.get_lwdata(no_reward_mat,option);

option.filename='reward_mat';
FLW_import_mat.get_lwdata(reward_mat,option);