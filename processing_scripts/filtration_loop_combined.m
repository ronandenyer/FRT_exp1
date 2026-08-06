close all

%% Participant details

base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\Combined_experiment'; 
pat_dir = strcat(base_dir, '\', pat);

files = dir(pat_dir); 
%% loop through files in the pat_dir
for k = 1:length(files)
    fname = files(k).name;
    fsize = files(k).bytes;

    if contains(fname, '.kinarm') && fsize > 15000*(10^3) && fsize < 60000*(10^3)
        filename = fullfile(files(k).folder, files(k).name); % complete path of the file
        block_data = exam_load(filename);
        block_data_filtered = filtration(block_data);
        block_name = strcat(pat, '_', block_data_filtered(1).EXPERIMENT.TASK_PROTOCOL, '.mat');
        disp(block_name);
        save_path = fullfile(files(k).folder, block_name);
        save(save_path, 'block_data_filtered');
    end
    
    
end
