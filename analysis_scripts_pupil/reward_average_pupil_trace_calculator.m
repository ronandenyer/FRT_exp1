%% Define Participant details
pat = 'RONAN';
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\reward_experiment'; 
pat_dir = strcat(base_dir, '\', pat);

reward_file = strcat(pat_dir, '\', pat, '_reward_reach_data_compact.mat');
load(reward_file);
reward_data = Compact_all;
%% Make table to store average values

pupil_avg = table();
pupil_avg.SID(1:2) = string(pat);
pupil_avg.reward_condition(1) = 3;
pupil_avg.reward_condition(2) = 4;

%% split single trial table according to stim condition and calcualte average trace of normalized pupil size

for i = 1:height(pupil_avg)

    % Build logical index for matching condition
    matchIdx = reward_data.reward_type == pupil_avg.reward_condition(i) & reward_data.pupil_nan_percent < 0.3 ;

    % --- normalized_eye1 ---
    cellArrays = reward_data.pupil_size_normalized(matchIdx);
    nonEmptyCells = cellfun(@(x) x', cellArrays(~cellfun(@isempty, cellArrays) & ...
                               ~cellfun(@(x) all(isnan(x)), cellArrays)), ...
                               'UniformOutput', false);
    % 1. Transpose and stack trials (Rows = Trials, Columns = Time points)
    numericData = cell2mat(nonEmptyCells);
    
    % 2. Calculate the mean for EACH TRIAL (averaging across the 10,000 points)
    % We use '2' to calculate the mean across the second dimension (columns)
    trialMeans = mean(numericData, 2);
    
    % 3. Identify extreme outliers based on the trial averages
    % 'isoutlier' identifies values more than 3 scaled MAD from the median by default.
    % You can use 'ThresholdFactor' to make it more or less sensitive.
    outlierIdx = isoutlier(trialMeans); 
    

    % Optional: Print how many trials were removed
    fprintf('Removing %d outlier trials from condition %d\n', sum(outlierIdx), i);
    
    % 4. Remove the outlier rows from the data matrix
    numericData = numericData(~outlierIdx, :);
    
    % 5. Calculate the Grand Average (Point-by-point mean of the clean trials)
    % We use '1' to average down the rows (across trials)
    averageResult = mean(numericData, 1);
    
    % Store the result
    pupil_avg.pupil_size_normalized{i} = averageResult;


    % % --- eye1 ---
    cellArrays = reward_data.pupil_size(matchIdx);
    nonEmptyCells = cellfun(@(x) x', cellArrays(~cellfun(@isempty, cellArrays) & ...
                               ~cellfun(@(x) all(isnan(x)), cellArrays)), ...
                               'UniformOutput', false);
    % 1. Transpose and stack trials (Rows = Trials, Columns = Time points)
    numericData = cell2mat(nonEmptyCells);
    
    % 2. Calculate the mean for EACH TRIAL (averaging across the 10,000 points)
    % We use '2' to calculate the mean across the second dimension (columns)
    trialMeans = mean(numericData, 2);
    
    % 3. Identify extreme outliers based on the trial averages
    % 'isoutlier' identifies values more than 3 scaled MAD from the median by default.
    % You can use 'ThresholdFactor' to make it more or less sensitive.
    outlierIdx = isoutlier(trialMeans); 
    
    % Optional: Print how many trials were removed
    fprintf('Removing %d outlier trials from condition %d\n', sum(outlierIdx), i);
    
    % 4. Remove the outlier rows from the data matrix
    numericData = numericData(~outlierIdx, :);
    
    % 5. Calculate the Grand Average (Point-by-point mean of the clean trials)
    % We use '1' to average down the rows (across trials)
    averageResult = mean(numericData, 1);
    
    % Store the result
    pupil_avg.pupil_size{i} = averageResult;

end



hold on
plot(pupil_avg.pupil_size{1}(1:7000), 'b')
plot(pupil_avg.pupil_size{2}(1:7000), 'r--')
hold off