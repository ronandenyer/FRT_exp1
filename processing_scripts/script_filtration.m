close all

%Dialog box to select the .kinarm file
[file, folder] = uigetfile('*.*', 'Select Kinarm file');
filename = fullfile(folder, file); % complete path of the file
data = exam_load(filename);

%Data filtrated 
block1 = filtration(data);          % CHANGE HERE (name)

%Dialog box to select the folder where the data will be saved 
[nomFichier, cheminFichier] = uiputfile('*.mat', 'Select the save folder');
if nomFichier == 0
    disp('Cancel');
else
    % full path of the file to save
    cheminComplet = fullfile(cheminFichier, nomFichier);
    
    % Save the data in the selected folder
    save(cheminComplet, 'block1');           % CHANGE HERE (2nd argument, same as name)
    
    disp(['Data successfully saved', cheminComplet]);
end
