%% calculate accuracy (%and RT) for each coherence
%% To check first: fill in the right file name, mixed blocks are the last 2 blocks & coherences are 0.1, 0.2, 0.4 ,0.6 and 0.8

%Step 1: extract the coherences and the accuracy for each trial. 
%------------------------------------------------------------------
% The script is made for training, where the blocs to compute the optimal
% coherences where the last ones (called mixed blocks).

%[file,folder] = uigetfile('*.*'); 
%filename=[folder file];
%data = readtable(filename);

%Extract all the data
clear all
all_data = exam_load('751*');  %%TO FILL WITH THE RIGHT FILE NAME
sz = size(all_data);
n = sz(1);
%Extract only the data from the mixed blocks (the last two lines of all data)
%opt_co_data1 = all_data(n-1).c3d;   
opt_co_data2 = all_data(n).c3d;

%Create the matrix with the trial position, its coherence and accuracy
%co_and_acc1 = characteristic(opt_co_data1);
co_and_acc2 = characteristic(opt_co_data2);
%co_and_acc_all = [co_and_acc1; co_and_acc2] %Concatenate the 2 blocks in 1 matrix


%Step2: calculate the mean accuracy for each coherence 
%------------------------------------------------------
% They are stored it in the first column of the matrix result

result = [0.1 0; 0.2 0 ; 0.4  0; 0.6 0; 0.8 0]; %matrix of 5x2 with the dirrecent coherences in the first column

%iterate for each coherences (6 different coherences) 
for i = 1:5                      
    coherence = result(i, 1);    

    %take the accuracy of every trial of coherence i, make the mean and store 
    % it in the 2nd column of result  
    index = find(co_and_acc2(:,2) == coherence);
    acc = co_and_acc2(index, 3);
    mean_acc = mean(acc, "all");
    result(i,2) = mean_acc;
end


%% fitting the Psychometric %and Chronometric function
% Define parameters
%--------------------
n = 48;                   % Number of trials in the blocs of the session (not training)
coherences = result(:,1)'; % Coherences of motion
%rt_means = result(:,2)'; % Mean response times for each coherence
acc_means = result(:,2)'; % Mean accuracies for each coherence

% Define the functions to fit
%-----------------------------
Psychometric_Accuracy = @(params, x) 1 ./ (1 + exp(-2 * params(1) * params(2) * abs(x)));
%Chronometric_RT = @(params, x) (params(1) ./ (params(2) * x)) .* tanh(params(1) * params(2) * x) + params(3);

% Define the likelihood function
%--------------------------------
%SEM_pred = std(rt_means)/sqrt(n);
%likelihood = @(params) -sum(log(normpdf(rt_means, Chronometric_RT(params, coherences), SEM)) + log(binopdf(round(acc_means*n), n, Psychometric_Accuracy(params, coherences))));
likelihood = @(params) -sum(log(binopdf(round(acc_means*n), n, Psychometric_Accuracy(params, coherences))));
% likelihood = @(params) -sum(log(normpdf(rt_means, Chronometric_RT(params, coherences), SEM_pred)));

% Find the maximum likelihood parameters
%---------------------------------------
starting_params = [15 0.5 100];
params = fminsearch(likelihood, starting_params);


% Plot the results
%------------------
figure;
%subplot(2,1,1);
%plot(coherences, rt_means, 'o', 'MarkerFaceColor', 'b');
%hold on;
%plot(coherences, Chronometric_RT(params, coherences), '-r');
%xlabel('Coherence');
%ylabel('Response time (s)');
%title('Chronometric function');
%legend('Data', 'Fit', 'Location', 'NorthWest');
%subplot(2,1,2);
plot(coherences, acc_means, 'o', 'MarkerFaceColor', 'b');
hold on;
plot(coherences, Psychometric_Accuracy(params, coherences), '-r');
xlabel('Coherence');
ylabel('Accuracy');
title('Psychometric function');
legend('Data', 'Fit', 'Location', 'SouthEast');

%% find the target coherence
% Define the target value
target_acc = 0.70;

% Define the objective function to minimize
target_func = @(x) abs(Psychometric_Accuracy(params, x) - target_acc);

% Find the coherence value that minimizes the objective function
starting_coherence = 0.1; % Starting point for the search
coherence_solution = fminsearch(target_func, starting_coherence);

disp(['Coherence solution = ', num2str(coherence_solution)]);

%%Function to use

%Extract the coherence and the accuracy and store it in a matrix coherence 200x3 
% (in the case where the mixed blocs have a length of 100 trials) where the fist column is 
% the position of the trial in the block, the second is the coherence and
% the third is the results of the trial (i.e succeed =1 or failed = 0). 
% INPUT : trial_data, which are the c3d data (i.e. all_data(n).c3d)

function matrix = characteristic(trial_data)

%Creation of the empty matrix
    sz = size(trial_data);
    n_trial = sz(2);
    co_and_acc = zeros(n_trial, 3);
    
    for i = 1:n_trial
        %Store the position of the trial in the block in the 1st column
        co_and_acc(i,1) = i;
        
        %check the position of the trial results in the labels array
        labels = trial_data(i).EVENTS.LABELS;  %cells array of size 1 x count
        s = size(labels);
        count = s(2);
        position = 0;
    
        for n = 1:count    %loop in the array labels
            if strncmpi(labels(1, n), 'left', 4) || strncmpi(labels(1, n), 'right', 5)
                position = n;
            elseif strncmpi(labels(1, n), 'Out of time', 4)
                position = 1; %because in this case, the loop that store the accuracy will put the accuracy to 0
            end
        end
        disp(position)
    
        %Store the accuracy of the trial i (i.e. 'left correct', ...) in the matrix 
        %(1 = success, 0 = failed trial)
        results = labels(position);
        if strncmpi(results, 'left correct', 10) || strncmpi(results, 'right correct', 10)
            co_and_acc(i,3) = 1;
        else 
            co_and_acc(i,3) = 0;
        end
    
        %Store the coherence of the trial (retrived in tp thanks to TP_ROW) in the 
        %2nd column of the matrix
        tp = trial_data(i).TRIAL.TP_ROW;
        if tp == 1 || tp == 2 || tp == 3 || tp == 4 || tp == 5 || tp == 6 || tp == 7 || tp == 8
            co_and_acc(i,2) = 0.8;
        elseif tp == 9 || tp == 10 || tp == 11 || tp == 12 || tp == 13 || tp == 14 || tp == 15 || tp == 16
            co_and_acc(i,2) = 0.6;
        elseif tp == 17 || tp == 18 || tp == 19 || tp == 20 || tp == 21 || tp == 22 || tp == 23 || tp == 24
            co_and_acc(i,2) = 0.4;
        elseif tp == 25 || tp == 26 || tp == 27 || tp == 28 || tp == 29 || tp == 30 || tp == 31 || tp == 32
            co_and_acc(i,2) = 0.2;
        elseif tp == 33 || tp == 34 || tp == 35 || tp == 36 || tp == 37 || tp == 38 || tp == 39 || tp == 40
            co_and_acc(i,2) = 0.1;
        end
    end
    matrix = co_and_acc;
end

