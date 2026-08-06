%AUTHOR: Florence Blondiaux
% 12 Apr 21 

function [TrialsPectoralis, TrialsDeltoid] =    filterEMG(Trialsi)
%filters the EMG data with a bandpass filter (20 - 250 Hz)
%+notch for the 50Hz frequences
%Input -> One trials structure containing the muscles named Pectoralis,
%Deltoide, Brachialis, Biceps, TricepsUn and TicepsDeux.

%Output -> A table with the 6 filtred muscle. (6*trial_length)
fs = 1000;

for notch_freq = [50 100 200 300]
    [b,a] = iirnotch(notch_freq/(fs/2),5/(fs/2)); %remove 50hz frequencies -> digital noise

    Trialsi.Pectoralis = abs(filtfilt(b,a,Trialsi.Pectoralis));
    Trialsi.Deltoide = abs(filtfilt(b,a,Trialsi.Deltoide));
    % Trialsi.Brachialis = abs(filtfilt(b,a,Trialsi.Brachialis));
    % Trialsi.Biceps = abs(filtfilt(b,a,Trialsi.Biceps));
    % Trialsi.Triceps = abs(filtfilt(b,a,Trialsi.Triceps));
    % Trialsi.TricepsDeux = abs(filtfilt(b,a,Trialsi.TricepsDeux));
end

[b,a] = butter(6, [20/(fs/2) 250/(fs/2)],'bandpass');

Trialsi.Pectoralis = abs(filtfilt(b,a,Trialsi.Pectoralis));
Trialsi.Deltoide = abs(filtfilt(b,a,Trialsi.Deltoide));
% Trialsi.Brachialis = abs(filtfilt(b,a,Trialsi.Brachialis));
% Trialsi.Biceps = abs(filtfilt(b,a,Trialsi.Biceps));
% Trialsi.Triceps = abs(filtfilt(b,a,Trialsi.Triceps));
% Trialsi.TricepsDeux = abs(filtfilt(b,a,Trialsi.TricepsDeux));


TrialsPectoralis = Trialsi.Pectoralis';
TrialsDeltoid = Trialsi.Deltoide';
    % Trialsi.Brachialis';
    % TrialsPectoralis = Trialsi.Biceps';
    % TrialsDeltoid = Trialsi.Triceps';
    % Trialsi.TricepsDeux'
end