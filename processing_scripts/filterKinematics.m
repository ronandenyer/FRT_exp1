%AUTHOR: Florence Blondiaux
% 12 Apr 21 
%Filters the trials, only the kinematics.
%Low pass filter 20 Hz on the kinematics
%Returns Trialsi, a structure, identical to the first one, but with filtred
%values.

function Trialsi= filterKinematics(Trialsi)

fs = 1000;
[b,a] = butter(4, 20/(fs/2) ,'low');

Trialsi.Right_HandX = filtfilt(b,a, Trialsi.Right_HandX);
Trialsi.Right_HandY = filtfilt(b,a,Trialsi.Right_HandY);
Trialsi.Right_FS_ForceX = filtfilt(b,a,Trialsi.Right_FS_ForceX);
Trialsi.Right_FS_ForceY = filtfilt(b,a,Trialsi.Right_FS_ForceY);
Trialsi.Right_FS_ForceZ = filtfilt(b,a,Trialsi.Right_FS_ForceZ);
Trialsi.Right_FS_TorqueX = filtfilt(b,a,Trialsi.Right_FS_TorqueX);
Trialsi.Right_FS_TorqueY = filtfilt(b,a,Trialsi.Right_FS_TorqueY);
Trialsi.Right_FS_TorqueZ = filtfilt(b,a,Trialsi.Right_FS_TorqueZ);

Trialsi.Right_HandXVel = filtfilt(b,a, Trialsi.Right_HandXVel);
Trialsi.Right_HandYVel = filtfilt(b,a,Trialsi.Right_HandYVel);

Trialsi.Right_HandXAcc = filtfilt(b,a, Trialsi.Right_HandXAcc);
Trialsi.Right_HandYAcc = filtfilt(b,a,Trialsi.Right_HandYAcc);

end