%% Define Participant details
pat = 'FRTP06';
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\reward_experiment'; 
pat_dir = strcat(base_dir, '\', pat);
reward_file = strcat(pat_dir, '\', pat, '_reward_reach_data_compact.mat');
load(reward_file);
reward_data = Compact_all;

%% Calculate previous trial success
% Shift the feedback_success column by 1. The first trial gets NaN since there is no previous trial.
reward_data.prev_trial_success = [NaN; reward_data.feedback_success(1:end-1)];
reward_data.prev_trial_reward = [NaN; reward_data.reward_type(1:end-1)];

%% Make table to store average values
% Create 2 rows for previous success (0 or 1)
pupil_avg = table();
pupil_avg.SID(1:2) = string(pat);
pupil_avg.prev_trial_success = [0; 1];

%% split single trial table according to prev_trial_success, then calculate average
for i = 1:height(pupil_avg)
    % 1. Find indices of trials matching the success condition & current trial quality
    valid_idx = find(reward_data.prev_trial_success == pupil_avg.prev_trial_success(i) & ...
                     reward_data.pupil_nan_percent < 0.3 & reward_data.prev_trial_reward == 3);
    
    % 2. Ensure previous trial has good data quality (< 30% NaNs)
    prev_quality_check = reward_data.pupil_nan_percent(valid_idx - 1) < 0.3;
    valid_idx = valid_idx(prev_quality_check);
    
    % NEW: Verify that BOTH the previous and current trial have at least 8000 points
    valid_length = arrayfun(@(idx) numel(reward_data.pupil_size{idx - 1}) >= 8000 && ...
                                   numel(reward_data.pupil_size{idx}) >= 8000, valid_idx);
    valid_idx = valid_idx(valid_length);
    
    % 3. Extract and concatenate previous and current trial (8000 points each)
    concatenated_trials = cell(length(valid_idx), 1);
    for k = 1:length(valid_idx)
        idx = valid_idx(k);
        
        % Slice the first 8000 points and reshape into row vectors
        prev_trial = reshape(reward_data.pupil_size{idx - 1}(1:8000), 1, []);
        curr_trial = reshape(reward_data.pupil_size{idx}(1:8000), 1, []);
        
        % Combine into a single 16000-point row
        concatenated_trials{k} = [prev_trial, curr_trial]; 
    end
    
    % 4. Convert to numeric matrix (Rows = Trials, Columns = 24000 Time points)
    numericData = cell2mat(concatenated_trials);
    
    % 5. Calculate the mean for EACH concatenated sequence (averaging across the 24,000 points)
    trialMeans = mean(numericData, 2, 'omitnan');
    
    % 6. Identify extreme outliers based on the trial averages
    outlierIdx = isoutlier(trialMeans); 
    
    % Optional: Print how many trial sequences were removed
    fprintf('Removing %d outlier sequences from PrevSuccess %d (Raw)\n', sum(outlierIdx), pupil_avg.prev_trial_success(i));
    
    % 7. Remove the outlier rows from the data matrix
    numericData = numericData(~outlierIdx, :);
    
    % 8. Calculate the Grand Average (Point-by-point mean of the clean sequences)
    averageResult = mean(numericData, 1, 'omitnan');
    
    % Store the result
    pupil_avg.pupil_size{i} = averageResult;
end

%% plotting
fig = figure('Color', 'w', 'Position', [100, 100, 900, 500]);
hold on

% Define styling arrays so they map sequentially to the 2 rows in pupil_avg
labels = {'Previous Trial: Fail', 'Previous Trial: Success'};
line_colors = {'b', 'r'};

for i = 1:height(pupil_avg)
    plot(pupil_avg.pupil_size{i}, ...
        'Color', line_colors{i}, ...
        'LineWidth', 1.5, ...
        'DisplayName', labels{i});
end

% Add a vertical line to denote the boundary between the previous and current trial
xline(8001, 'k--', 'LineWidth', 1.5, 'HandleVisibility', 'off'); 

% Add background annotations for clarity (optional, grabs current ylim to place text)
y_limits = ylim;
text(6000, y_limits(2), 'Previous Trial', 'HorizontalAlignment', 'center', 'VerticalAlignment', 'top', 'FontSize', 12, 'FontWeight', 'bold');
text(18000, y_limits(2), 'Current Trial', 'HorizontalAlignment', 'center', 'VerticalAlignment', 'top', 'FontSize', 12, 'FontWeight', 'bold');

hold off
title('Raw Pupil Size: Previous Trial into Current Trial')
xlabel('Time Points')
ylabel('Pupil Size')
legend('Location', 'best')