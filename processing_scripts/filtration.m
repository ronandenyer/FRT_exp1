function data_filtered = filtration(dataIn)
% dataIn are the raw data of one bloc

Trials = [dataIn(1).c3d];
%listTP = extractTP(Blocs);
Trials = KINARM_add_hand_kinematics(Trials);% Add hand velocity, acceleration and commanded forces to the data structure

for i = 1:length(Trials)
    Trials(i) = filterKinematics(Trials(i));	% Double-pass filter the data at 3db cutoff frequency of 10 Hz.  Use an enhanced method for reducing transients at ends.
    
end


TrialsL = rmfield(Trials, {'Left_HandX','Left_HandY','Left_HandXVel','Left_HandYVel', ...
            'Left_HandXAcc','Left_HandYAcc', ...
            'Right_L1Ang', 'Right_L2Ang', 'Right_L1Vel','Right_L2Vel', 'Right_L1Acc', 'Right_L2Acc', ...
            'Right_M1TorCMD', 'Right_M2TorCMD', ...
            'Right_FS_TorqueX','Right_FS_TorqueY','Right_FS_TorqueZ','Right_FS_TimeStamp','Right_FS_Status',...
            'Left_L1Ang', 'Left_L2Ang', 'Left_L1Vel','Left_L2Vel', 'Left_L1Acc', 'Left_L2Acc', ...
            'Left_M1TorCMD', 'Left_M2TorCMD','Left_FS_ForceX','Left_FS_ForceY','Left_FS_ForceZ', ...
            'Left_FS_TorqueX','Left_FS_TorqueY','Left_FS_TorqueZ','Left_FS_TimeStamp','Left_FS_Status',...
            'Gaze_X','Gaze_Y','Gaze_TimeStamp','Gaze_Vector_X','Gaze_Vector_Y', ...
            'Right_Hand_ForceCMD_Y', 'Right_Hand_ForceCMD_X',  'Left_Hand_ForceCMD_Y', 'Left_Hand_ForceCMD_X', ...
            'Gaze_Vector_Z','Gaze_Pupil_X','Gaze_Pupil_Y','Gaze_Pupil_Z','StatusBits','ANALOG','HAND', ...
            'VIDEO_LATENCY','ACCESSORIES','BLOCK_TABLE','CALIBRATION','EVENT_DEFINITIONS',...
            'LEFT_KINARM','MANUFACTURER','RIGHT_KINARM','TARGET_TABLE', ...
            'TORQUE_MOTOR','USER_CHANNELS','VIDEO_SETTINGS', 'IMAGE_TABLE', 'LOAD_TABLE', 'FILE_NAME', });

%OLD - code Astrid
%--------------------
%filtre et met les données cinématiques en coordonnées X-Y
%Crenna et al.F, Filtering Biomechanical Signals in Movement Analysis. Sensors. 2021; 21(13):4580. https://doi.org/10.3390/s21134580
%kinematics_XY = KINARM_add_hand_kinematics(dataIn);
%kinematics_filtered = filter_double_pass_modified(kinematics_XY, 'kinematics', 'enhanced', 'lowpass', 'fc', 10); 
%Filtre les données musculaires (passband de 20-250Hz)
%Selen: 0 and 450 Hz, Codol: 20 and 250 Hz, DeComité: 20 and 250Hz 
%muscle_high = filter_double_pass_modified(kinematics_filtered, 'muscles', 'enhanced', 'highpass', 'fc', 20);
%data = filter_double_pass_modified(muscle_high, 'muscles', 'enhanced','lowpass', 'fc', 250);
%pupil_data = filter_double_pass_modified(dataIn, 'pupil', 'enhanced', '', 'fc', );

data_filtered = TrialsL;
end



