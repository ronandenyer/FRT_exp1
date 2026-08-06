%Author: Florence Blondiaux
%Date :March 2021
%First file to run to extract the data.

close all
addpath '/home/fblondiaux/Documents/GitHub/MasterThesis/KinarmCode/KinarmScripts-and-docs-master/'
script_directory =  '/home/fblondiaux/Documents/GitHub/MasterThesis/PerturbHandStill/scripts';
addpath(script_directory)
data_directory = '/media/fblondiaux/T7Shield/ETResearchData/';
%% Loop on populations

for pop = ["Controls","Patients"]
    %Get calibration
    forcerecompute= false;
    extractRawCalib(append(data_directory,pop),forcerecompute);
    extractLightMatrixCalib(append(data_directory,pop),forcerecompute);
    %Get trials
    extractRaw(append(data_directory,pop),forcerecompute);
    extractLightMatrix(append(data_directory,pop),forcerecompute);
    %Get baseline
    extractRawBaseline(append(data_directory,pop), forcerecompute);
    extractLightMatrixBaseline(append(data_directory,pop),forcerecompute);
end


cd(script_directory)

%% Functions definition

function extractRaw(superdirectory, forcerecompute)
%Gets the raw files, filters and save.

%INPUT: - superdirectory = append(data_directory, pop)
%       - forcecompute = false (par défaut)         

cd(superdirectory)           %Change le répertoire de travail 
subFolders = dir();
subFolders(ismember( {subFolders.name}, {'.', '..'})) = []; %remove . and ..
subFolders(~[subFolders.isdir]) = [];  %remove non-directories
subFolders = removeNonParticipantsFC(subFolders,superdirectory);

for z = 1 : length(subFolders)
    name = subFolders(z).name;
    directory = append(superdirectory,'/', name,'/FC01/Kinarm/');
    cd(directory);
    if ~isfile(append(superdirectory,'/', name, '/FC01/Kinarm/Trials.mat')) || forcerecompute
        
        Trials = exam_load; %load all the .zip and .kinarm files from the folder
        
        Trials = [Trials(1).c3d Trials(2).c3d Trials(3).c3d Trials(4).c3d Trials(5).c3d];
        C = load('CalibCoef.mat');
        CalibCoef = C.CalibCoef;
        listTP = extractTP(Trials);
        Trials = KINARM_add_hand_kinematics(Trials);% Add hand velocity, acceleration and commanded forces to the data structure

        for i = 1:length(Trials)
            Trials(i) = filterKinematics(Trials(i));	% Double-pass filter the data at 3db cutoff frequency of 10 Hz.  Use an enhanced method for reducing transients at ends.
            %Treat EMG differently
            Trials(i).EMG = filterEMG(Trials(i))./CalibCoef;
            Trials(i).listTP = listTP;
        end
        
        
        save(append(superdirectory,'/', name, '/FC01/Kinarm/Trials.mat'),'Trials');
        
    end
end

end


function extractLightMatrix(superdirectory,forceRecompute)
%From the filtered data, we keep the following variables:
% {'Right_HandX'     }
% {'Right_HandY'     }
% {'Right_HandXVel'  }
% {'Right_HandYVel'  }
% {'Right_HandXAcc'  }
% {'Right_HandYAcc'  }
% {'Right_FS_ForceX' }
% {'Right_FS_ForceY' }
% {'Right_FS_ForceZ' }
% {'Right_FS_TorqueX'}
% {'Right_FS_TorqueY'}
% {'Right_FS_TorqueZ'}
% {'EVENTS'          }
% {'TRIAL'           }
% {'FILE_NAME'       }
% + Mov field that contains the indexes of the begining and the end of the
% movement.

%NB fails is the number of movements that started but never stopped for
%participants.

cd(superdirectory)
subFolders = dir();
subFolders(ismember( {subFolders.name}, {'.', '..'})) = []; %remove . and ..
subFolders(~[subFolders.isdir]) = [];  %remove non-directories
subFolders = removeNonParticipantsFC(subFolders,superdirectory);
const
for z = 1 : length(subFolders)
    clear Trials TrialsL
    name = subFolders(z).name;
    directory = append(superdirectory,'/', name,'/FC01/Kinarm/');
    cd(directory);
    if ~isfile('TrialsL.mat') || forceRecompute
        %load the row matrix.
        S = load('Trials.mat');
        Trials = S.Trials;
        
        %removes useless fields
        TrialsL = rmfield(Trials, {'Left_HandX','Left_HandY','Left_HandXVel','Left_HandYVel', ...
            'Left_HandXAcc','Left_HandYAcc','Left_Hand_ForceCMD_X','Left_Hand_ForceCMD_Y', ...
            'Left_L1Ang', 'Left_L2Ang', 'Left_L1Vel','Left_L2Vel', 'Left_L1Acc', 'Left_L2Acc', ...
            'Left_M1TorCMD', 'Left_M2TorCMD','Left_FS_ForceX','Left_FS_ForceY','Left_FS_ForceZ', ...
            'Left_FS_TorqueX','Left_FS_TorqueY','Left_FS_TorqueZ','Left_FS_TimeStamp','Left_FS_Status',...
            'Gaze_X','Gaze_Y','Gaze_TimeStamp','Gaze_PupilArea','Gaze_Vector_X','Gaze_Vector_Y', ...
            'Gaze_Vector_Z','Gaze_Pupil_X','Gaze_Pupil_Y','Gaze_Pupil_Z','StatusBits','ANALOG','HAND', ...
            'Pectoralis','Deltoide','Brachialis','Biceps','TricepsUn','TricepsDeux',...
            'VIDEO_LATENCY','ACCESSORIES','BLOCK_TABLE','CALIBRATION','EVENT_DEFINITIONS',...
            'LEFT_KINARM','MANUFACTURER','RIGHT_KINARM','TARGET_TABLE', ...
            'TORQUE_MOTOR','TP_TABLE','USER_CHANNELS','VIDEO_SETTINGS'});
        
        for ii = 1:length(TrialsL)
            
%             %Find the indexes of the movement from the events.
%             
%             %Find the moment where the perturbation was applied and
%             %stopped.
%             
%             closeToForceStart = floor(TrialsL(ii).EVENTS.TIMES(find(contains(TrialsL(ii).EVENTS.LABELS,'Load on'),1)) * 1000)-30; %Get a guess of the index
%             diffIDX = find(abs(TrialsL(ii).Right_Hand_ForceCMD_X(closeToForceStart:end))>0.1,1); %Look for the precise moment it started
%             if isempty(diffIDX)
%                 diffIDX = find(abs(TrialsL(ii).Right_Hand_ForceCMD_Y(closeToForceStart:end))>0.1,1);
%             end
%             index_start = closeToForceStart+diffIDX-2; %Index found when force > 0 , then we go back from one to be just before it started
%             
%             if ~isempty(index_start)
%                 
%                 vit = sqrt(TrialsL(ii).Right_HandYVel.^2 + TrialsL(ii).Right_HandXVel.^2)';
%                 mov_s = index_start(1); % index of the begining of period of trial = start of perturbation
%                 %looking for movement' end
%                 cand = strfind(vit>0.03,zeros(1,200)); % when is the movement stopping ? 3 cm/s thresh
%                 cand = cand(cand>mov_s);
%                 if isempty(cand)
%                     %Movement never stops
%                     mov_s = 0;
%                     mov_e = 0;
%                 else
%                     
%                     mov_e = cand(1); % index of the end of the movement
%                 end
%                 
%             else  %No go signal
%                 mov_s = 0;
%                 mov_e = 0;
%             end
%             TrialsL(ii).Mov = [mov_s mov_e];
%Find the indexes of the movement from the events.
            
            %Find the moment where the perturbation was applied and
            %stopped.
            closeToForceStart = floor(TrialsL(ii).EVENTS.TIMES(find(contains(TrialsL(ii).EVENTS.LABELS,'Load on'),1)) * 1000)-30; %Get a guess of the index
            diffIDX = find(abs(TrialsL(ii).Right_Hand_ForceCMD_X(closeToForceStart:end))>0.1,1); %Look for the precise moment it started
            if isempty(diffIDX)
                diffIDX = find(abs(TrialsL(ii).Right_Hand_ForceCMD_Y(closeToForceStart:end))>0.1,1);
            end
            index_start = closeToForceStart+diffIDX-2; %Index found when force > 0 , then we go back from one to be just before it started
            
            if ~isempty(index_start)               
                
                vit = sqrt(TrialsL(ii).Right_HandYVel.^2 + TrialsL(ii).Right_HandXVel.^2)';
                start_t = index_start(1); % index of the begining of period of trial = start of perturbation
                
                %looking for movement' end
                    %Check when the person is back to the target, then
                    distCenter = sqrt((TrialsL(ii).Right_HandX-centerX).^2+(TrialsL(ii).Right_HandY-centerY).^2)';
                    %check if they managed to stop.
                    
                    targetLeft = strfind(distCenter<0.01,zeros(1,10)); % hand left the target
                    targetLeft = targetLeft(targetLeft>start_t);
                    if isempty(targetLeft)
                        %Movement never left the target
                        crossed_t = start_t+1;%Instantaneously crossed the target
                        cand = strfind(vit<0.03,ones(1,200));
                        cand = cand(cand>crossed_t);
                        if isempty(cand)
                                %Movements never stops
                                stop_t = 0;
                        else
                                stop_t = cand(1); % index of the end of the movement
                        end
                        
                    else
                        %Hand left the target does it comes back
                        targetLeft = targetLeft(1);
                        targetCrossed = strfind(distCenter<0.01,ones(1,10)); % hand back to the target
                        targetCrossed = targetCrossed(targetCrossed>targetLeft);
                        
                        if isempty(targetCrossed)
                            %participant didn't came back.
                            start_t = 0;
                            stop_t = 0;
                            crossed_t = 0;
                        else
                            crossed_t = targetCrossed(1);
                            cand = strfind(vit<0.03,ones(1,200));
                            cand = cand(cand>crossed_t);
                            if isempty(cand)
                                %Movements never stops
                                stop_t = 0;
                            else
                                stop_t = cand(1); % index of the end of the movement
                            end
                            
                        end
                        
                    end
                else %perturbation not activated
                    start_t = 0;
                    stop_t = 0;
                    crossed_t = 0;
                end
                
                
            TrialsL(ii).start_t = start_t;
            TrialsL(ii).stop_t = stop_t;
            TrialsL(ii).crossed_t = crossed_t;
                   
        end
        save('TrialsL.mat','TrialsL')
        TrialsLnoEMG = rmfield(TrialsL, {'EMG'});
        save('TrialsLnoEMG.mat','TrialsLnoEMG')
        % delete Trials.mat % Removes the "heavy" one
    end
    disp(append("Done with ",name(1:6)))
end
end



function extractRawBaseline(superdirectory, forcerecompute)
%Gets the raw files, filters and save.

cd(superdirectory)
subFolders = dir();
subFolders(ismember( {subFolders.name}, {'.', '..'})) = []; %remove . and ..
subFolders(~[subFolders.isdir]) = [];  %remove non-directories
subFolders = removeNonParticipantsFC(subFolders,superdirectory);

for z = 1 : length(subFolders)
    name = subFolders(z).name;
    directory = append(superdirectory,'/', name,'/FC01/Kinarm/Baseline');
    cd(directory);
    if ~isfile(append(superdirectory,'/', name, '/FC01/Kinarm/Baseline/Trials.mat')) || forcerecompute
        
        Trials = exam_load; %load all the .zip and .kinarm files from the folder
        
        Trials = [Trials(1).c3d];
        C = load('../CalibCoef.mat');
        CalibCoef = C.CalibCoef;
        listTP = extractTPCalib(Trials,15);
                Trials = KINARM_add_hand_kinematics(Trials);% Add hand velocity, acceleration and commanded forces to the data structure

        for i = 1:length(Trials)
            Trials(i) = filterKinematics(Trials(i));	% Double-pass filter the data at 3db cutoff frequency of 10 Hz.  Use an enhanced method for reducing transients at ends.
            %Treat EMG differently
            Trials(i).EMG = filterEMG(Trials(i))./CalibCoef;
            Trials(i).listTP = listTP;
        end
        
        
        save(append(superdirectory,'/', name, '/FC01/Kinarm/Baseline/Trials.mat'),'Trials');
        
    end
end

end


function extractLightMatrixBaseline(superdirectory,forceRecompute)
%From the filtered data, we keep the following variables:
% {'Right_HandX'     }
% {'Right_HandY'     }
% {'Right_HandXVel'  }
% {'Right_HandYVel'  }
% {'Right_HandXAcc'  }
% {'Right_HandYAcc'  }
% {'Right_FS_ForceX' }
% {'Right_FS_ForceY' }
% {'Right_FS_ForceZ' }
% {'Right_FS_TorqueX'}
% {'Right_FS_TorqueY'}
% {'Right_FS_TorqueZ'}
% {'EVENTS'          }
% {'TRIAL'           }
% {'FILE_NAME'       }
% + Mov field that contains the indexes of the begining and the end of the
% movement.

%NB fails is the number of movements that started but never stopped for
%participants.

cd(superdirectory)
subFolders = dir();
subFolders(ismember( {subFolders.name}, {'.', '..'})) = []; %remove . and ..
subFolders(~[subFolders.isdir]) = [];  %remove non-directories
subFolders = removeNonParticipantsFC(subFolders,superdirectory);
const
for z = 1 : length(subFolders)
    clear Trials TrialsL
    name = subFolders(z).name;
    directory = append(superdirectory,'/', name,'/FC01/Kinarm/Baseline');
    cd(directory);
    if ~isfile('TrialsL.mat') || forceRecompute
        %load the row matrix.
        S = load('Trials.mat');
        Trials = S.Trials;
        
        %removes useless fields
        TrialsL = rmfield(Trials, {'Left_HandX','Left_HandY','Left_HandXVel','Left_HandYVel', ...
            'Left_HandXAcc','Left_HandYAcc','Left_Hand_ForceCMD_X','Left_Hand_ForceCMD_Y', ...
            'Left_L1Ang', 'Left_L2Ang', 'Left_L1Vel','Left_L2Vel', 'Left_L1Acc', 'Left_L2Acc', ...
            'Left_M1TorCMD', 'Left_M2TorCMD','Left_FS_ForceX','Left_FS_ForceY','Left_FS_ForceZ', ...
            'Left_FS_TorqueX','Left_FS_TorqueY','Left_FS_TorqueZ','Left_FS_TimeStamp','Left_FS_Status',...
            'Gaze_X','Gaze_Y','Gaze_TimeStamp','Gaze_PupilArea','Gaze_Vector_X','Gaze_Vector_Y', ...
            'Gaze_Vector_Z','Gaze_Pupil_X','Gaze_Pupil_Y','Gaze_Pupil_Z','StatusBits','ANALOG','HAND', ...
            'Pectoralis','Deltoide','Brachialis','Biceps','TricepsUn','TricepsDeux',...
            'VIDEO_LATENCY','ACCESSORIES','BLOCK_TABLE','CALIBRATION','EVENT_DEFINITIONS',...
            'LEFT_KINARM','MANUFACTURER','RIGHT_KINARM','TARGET_TABLE', ...
            'TORQUE_MOTOR','TP_TABLE','USER_CHANNELS','VIDEO_SETTINGS'});
        
        for ii = 1:length(TrialsL)
            
            %Find the indexes of the movement from the events.
            
            %Find the moment where the perturbation was applied and
            %stopped.
            
            targetOn = floor(TrialsL(ii).EVENTS.TIMES(find(contains(TrialsL(ii).EVENTS.LABELS,'TARGET_ON'),1)) * 1000); %Get a guess of the index
            if ~isempty(targetOn)
                
                vit = sqrt(TrialsL(ii).Right_HandYVel.^2 + TrialsL(ii).Right_HandXVel.^2)';
                start_t = targetOn(1); % index of the begining of period of trial = start of perturbation
                cand = strfind(vit>0.03,ones(1,10)); % when is the movement starting ? 3 cm/s thresh
                cand = cand(cand>start_t);
                if ~isempty(cand)
                    start_t = cand(1); %Movement start.
                    
                    %looking for movement' end
                    %Check when the person is back to the target, then
                    distCenter = sqrt((TrialsL(ii).Right_HandX-centerX).^2+(TrialsL(ii).Right_HandY-centerY).^2)';
                    %check if they managed to stop.
                    
                    targetLeft = strfind(distCenter<0.01,zeros(1,10)); % hand left the target
                    targetLeft = targetLeft(targetLeft>start_t);
                    if isempty(targetLeft)
                        %Movement never left the target
                        start_t = 0;
                        stop_t = 0;
                        crossed_t = 0;
                        neverLeftTODO = neverLeftTODO+1;
                    else
                        %Hand left the target does it comes back
                        targetLeft = targetLeft(1);
                        targetCrossed = strfind(distCenter<0.01,ones(1,10)); % hand left the target
                        targetCrossed = targetCrossed(targetCrossed>targetLeft);
                        
                        if isempty(targetCrossed)
                            %participant didn't came back.
                            start_t = 0;
                            stop_t = 0;
                            crossed_t = 0;
                        else
                            crossed_t = targetCrossed(1);
                            cand = strfind(vit<0.03,ones(1,200));
                            cand = cand(cand>crossed_t);
                            if isempty(cand)
                                %Movements never stops
                                stop_t = 0;
                            else
                                stop_t = cand(1); % index of the end of the movement
                            end
                            
                        end
                        
                    end
                else %person didn't move
                    start_t = 0;
                    stop_t = 0;
                    crossed_t = 0;
                end
                
                
            else  %No go signal
                start_t = 0;
                stop_t = 0;
                crossed_t = 0;
            end
            TrialsL(ii).start_t = start_t;
            TrialsL(ii).stop_t = stop_t;
            TrialsL(ii).crossed_t = crossed_t;
            
        end
        save('TrialsL.mat','TrialsL')
        % delete Trials.mat % Removes the "heavy" one
    end
    disp(append("Done with ",name(1:6)))
end
end


function extractRawCalib(superdirectory, forcerecompute)
%Gets the raw files, filters and save.

cd(superdirectory)
subFolders = dir();
subFolders(ismember( {subFolders.name}, {'.', '..'})) = []; %remove . and ..
subFolders(~[subFolders.isdir]) = [];  %remove non-directories
subFolders = removeNonParticipantsFC(subFolders,superdirectory);


for z = 1 : length(subFolders)
    clear data data_filt
    name = subFolders(z).name;
    directory = append(superdirectory,'/', name,'/FC01/Kinarm/Calib');
    cd(directory);
    if ~isfile(append(superdirectory,'/', name, '/FC01//Kinarm/TrialsCalib.mat')) || forcerecompute
        
        Trials = exam_load; %load all the .zip and .kinarm files from the folder
        %    data = reOrder(newTrials); %reOrder the trials the old way (when I was using c3d load).
        Trials = [Trials(1).c3d Trials(2).c3d Trials(3).c3d];
        
        listTP = extractTPCalib(Trials,9);
        Trials = KINARM_add_hand_kinematics(Trials);% Add hand velocity, acceleration and commanded forces to the data structure

        for i = 1:length(Trials)
            Trials(i) = filterKinematics(Trials(i));	% Double-pass filter the data at 3db cutoff frequency of 10 Hz.  Use an enhanced method for reducing transients at ends.
            %Treat EMG differently
            Trials(i).EMG = filterEMG(Trials(i));
            Trials(i).listTP = listTP;
        end
        
        
        save(append(superdirectory,'/', name, '/FC01/Kinarm/TrialsCalib.mat'),'Trials');
        
    end
end
end

function extractLightMatrixCalib(superdirectory,forceRecompute)
%From the filtered data, we keep the following variables:
% {'Right_HandX'     }
% {'Right_HandY'     }
% {'Right_HandXVel'  }
% {'Right_HandYVel'  }
% {'Right_HandXAcc'  }
% {'Right_HandYAcc'  }
% {'Right_FS_ForceX' }
% {'Right_FS_ForceY' }
% {'Right_FS_ForceZ' }
% {'Right_FS_TorqueX'}
% {'Right_FS_TorqueY'}
% {'Right_FS_TorqueZ'}
% {'EVENTS'          }
% {'TRIAL'           }
% {'FILE_NAME'       }
% + Mov field that contains the indexes of the begining and the end of the
% movement.

%NB fails is the number of movements that started but never stopped for
%participants.

cd(superdirectory)
subFolders = dir();
subFolders(ismember( {subFolders.name}, {'.', '..'})) = []; %remove . and ..
subFolders(~[subFolders.isdir]) = [];  %remove non-directories
subFolders = removeNonParticipantsFC(subFolders,superdirectory);

up_thresh = 16; %Force thresholds used to take or not recordings as calib data
low_thresh = 14;

for z = 1 : length(subFolders)
    clear Trials TrialsL
    name = subFolders(z).name;
    directory = append(superdirectory,'/', name,'/FC01/Kinarm/');
    cd(directory);
    
    
    
    %If already there, skip unless if force recomputes.
    if isfile('TrialsCalibL.mat') && forceRecompute == false
        continue
    end
    %load the raw matrix.
    S = load('TrialsCalib.mat');
    TrialsCalib = S.Trials;
    TrialsCalibL = rmfield(TrialsCalib, {'Left_HandX','Left_HandY','Left_HandXVel','Left_HandYVel', ...
        'Left_HandXAcc','Left_HandYAcc','Left_Hand_ForceCMD_X','Left_Hand_ForceCMD_Y', ...
        'Left_L1Ang', 'Left_L2Ang', 'Left_L1Vel','Left_L2Vel', 'Left_L1Acc', 'Left_L2Acc', ...
        'Left_M1TorCMD', 'Left_M2TorCMD','Left_FS_ForceX','Left_FS_ForceY','Left_FS_ForceZ', ...
        'Left_FS_TorqueX','Left_FS_TorqueY','Left_FS_TorqueZ','Left_FS_TimeStamp','Left_FS_Status',...
        'Gaze_X','Gaze_Y','Gaze_TimeStamp','Gaze_PupilArea','Gaze_Vector_X','Gaze_Vector_Y', ...
        'Gaze_Vector_Z','Gaze_Pupil_X','Gaze_Pupil_Y','Gaze_Pupil_Z','StatusBits','ANALOG','HAND', ...
        'VIDEO_LATENCY','ACCESSORIES','BLOCK_TABLE','CALIBRATION','EVENT_DEFINITIONS',...
        'LEFT_KINARM','MANUFACTURER','RIGHT_KINARM','TARGET_TABLE', ...
        'TORQUE_MOTOR','TP_TABLE','USER_CHANNELS','VIDEO_SETTINGS'});
    
    
    CalibVal = NaN(length(TrialsCalibL),6);
    
    for ii = 1:length(TrialsCalibL)
        
        %Find the indexes of the calibration. We only take the values,when
        %the hand force is between up_thresh and low_thresh
        
        MovIdx = find(sqrt(TrialsCalibL(ii).Right_FS_ForceX.^2+TrialsCalibL(ii).Right_FS_ForceY.^2)> low_thresh & sqrt(TrialsCalibL(ii).Right_FS_ForceX.^2+TrialsCalibL(ii).Right_FS_ForceY.^2)<up_thresh);
        TrialsCalibL(ii).MovIdx = MovIdx;
        CalibVal(ii,:) = nanmean(TrialsCalibL(ii).EMG(:,MovIdx),2)';
        
    end
    
    %Computes the coefficients based on the activation of muscles:
    
    listTP = TrialsCalibL(1).listTP;
    CalibCoef = [nanmean(CalibVal(listTP(3,:),1));%Pectoralis = TP3
        nanmean(CalibVal(listTP(1,:),2));        %Delto = TP1
        nanmean(CalibVal(listTP(4,:),3));%Brachio = TP4
        nanmean(CalibVal(listTP(4,:),4));%Biceps = TP4
        nanmean(CalibVal(listTP(2,:),5));%Triceps 1 = TP2
        nanmean(CalibVal(listTP(2,:),6)) % Triceps 2 = TP2
        ];
    
    
    for ii = 1:length(TrialsCalibL)
        TrialsCalibL(ii).EMG = TrialsCalibL(ii).EMG./CalibCoef;
    end
    
    %Save The Matrices
    save('CalibCoef.mat','CalibCoef')
    save('TrialsCalibL.mat','TrialsCalibL')
    
end

end

function listTP = extractTPCalib(Trials,nbRep)
%Returns a matrix with the index of each set of trials:
%eg : listTP(1,:) = all the indices (9) of the TP1.
listTP = NaN(4,nbRep);
count = [1 1 1 1];

for i = 1:length(Trials)
    TP = Trials(i).TRIAL.TP;
    listTP(TP,count(TP)) = i;
    count(TP) = count(TP)+1;
end
end

function listTP = extractTP(Trials)
%Returns a matrix with the index of each set of trials:
%eg : listTP(1,:) = all the indices (9) of the TP1.
listTP = NaN(12,9);
count = ones(1,12);

for i = 1:length(Trials)
    TP = Trials(i).TRIAL.TP;
    listTP(TP,count(TP)) = i;
    count(TP) = count(TP)+1;
end
end
