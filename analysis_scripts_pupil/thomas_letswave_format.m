%% Load data

data = readtable('mean_pupil_by_condition_ALL-10.csv');

subjects = unique(data.Subject);
num_pats = length(subjects);
epoch_length = 12001;
stim_type = ["sham", "tVNS"];
coherence = ["_0.6", "_0.7", "_0.8"];

%% initialize matrices


tVNS_6_mat = zeros(num_pats, epoch_length);
sham_6_mat = zeros(num_pats, epoch_length);

tVNS_7_mat = zeros(num_pats, epoch_length);
sham_7_mat = zeros(num_pats, epoch_length);

tVNS_8_mat = zeros(num_pats, epoch_length);
sham_8_mat = zeros(num_pats, epoch_length);

%% loop through each participant

for i = 1:num_pats

    pat = subjects(i); % set participant to be analyzed

    for j = 1:length(stim_type) % loop through stim types

        for k = 1:length(coherence) % loop through coherences for each stim_type
            condition = strcat(stim_type(j), coherence(k)); % set conditon based on loop
            pat_data = data{data.Subject == pat & data.Condition == condition,3:end}; % index big dataset to the specfic subject and condition and save in pat_data (ignore first 2 columns as they are Subject and Condition)  
            
            if condition == "sham_0.6" % place into the correct matrix based on current condition
                sham_6_mat(i,:) = pat_data;
            elseif condition == "sham_0.7"
                sham_7_mat(i,:) = pat_data;
            elseif condition == "sham_0.8"
                sham_8_mat(i,:) = pat_data;
            elseif condition == "tVNS_0.6"
                tVNS_6_mat(i,:) = pat_data;
            elseif condition == "tVNS_0.7"
                tVNS_7_mat(i,:) = pat_data;
            elseif condition == "tVNS_0.8"
                tVNS_8_mat(i,:) = pat_data;
            end

        end
    end

end

%% Change into letswave format for t-test analyses
LW_init();
option.dimension_descriptors={'epochs','X','channels','Y'};
option.unit='amplitude';
option.xunit='time';
option.yunit='frequency';
option.xstart=-2; % where does the pupil data start before tVNS onset?
option.xstep=0.001; % millisecond resolution
option.ystart=0;
option.ystep=1;
option.is_save=1;


option.filename='sham_0.6_mat';
FLW_import_mat.get_lwdata(sham_6_mat,option);

option.filename='tVNS_0.6_mat';
FLW_import_mat.get_lwdata(tVNS_6_mat,option);

option.filename='sham_0.7_mat';
FLW_import_mat.get_lwdata(sham_7_mat,option);

option.filename='tVNS_0.7_mat';
FLW_import_mat.get_lwdata(tVNS_7_mat,option);

option.filename='sham_0.8_mat';
FLW_import_mat.get_lwdata(sham_7_mat,option);

option.filename='tVNS_0.8_mat';
FLW_import_mat.get_lwdata(tVNS_7_mat,option);


