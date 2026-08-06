%% Define Participant details
pat = 'FRTC05'; % Update to your combined experiment subject ID
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\Combined_experiment'; 
pat_dir = fullfile(base_dir, pat);

data_file = fullfile(pat_dir, [pat, '_combined_reach_data_compact.mat']);
load(data_file);
data = Compact_all;

%% Make table to store average values for the 4 conditions
pupil_avg = table();
pupil_avg.SID(1:4) = string(pat);
pupil_avg.condition_id = (1:4)';
pupil_avg.cond_name = {'Sham_NoRew', 'Sham_Rew', 'tVNS_NoRew', 'tVNS_Rew'}';

% Map the 2x2 design conditions
target_stim = [1, 1, 2, 2];
target_rew  = [3, 4, 3, 4];

%% Split single trial table according to 2x2 conditions and calculate average trace
for i = 1:height(pupil_avg)

    % Build logical index for matching both stim and reward condition, plus NaN filter
    matchIdx = data.stim_type == target_stim(i) & ...
               data.reward_type == target_rew(i) & ...
               data.pupil_nan_percent < 0.3;

    % Define the fields to process (normalized and raw)
    fields = {'pupil_size_normalized', 'pupil_size'};
    
    for f = 1:length(fields)
        currentField = fields{f};
        
        cellArrays = data.(currentField)(matchIdx);
        nonEmptyCells = cellfun(@(x) x', cellArrays(~cellfun(@isempty, cellArrays) & ...
                                   ~cellfun(@(x) all(isnan(x)), cellArrays)), ...
                                   'UniformOutput', false);
                                   
        if isempty(nonEmptyCells)
            warning('No valid trials found for condition %d (%s), field: %s', i, pupil_avg.cond_name{i}, currentField);
            pupil_avg.(currentField){i} = [];
            continue;
        end
        
        % 1. Transpose and stack trials (Rows = Trials, Columns = Time points)
        numericData = cell2mat(nonEmptyCells);
        
        % 2. Calculate the mean for EACH TRIAL (averaging across the time points)
        trialMeans = mean(numericData, 2);
        
        % 3. Identify extreme outliers based on the trial averages
        outlierIdx = isoutlier(trialMeans); 
        
        % Optional: Print how many trials were removed
        fprintf('Removing %d outlier trials from condition %d (%s) for %s\n', ...
                sum(outlierIdx), i, pupil_avg.cond_name{i}, currentField);
        
        % 4. Remove the outlier rows from the data matrix
        numericData = numericData(~outlierIdx, :);
        
        % 5. Calculate the Grand Average (Point-by-point mean of the clean trials)
        averageResult = mean(numericData, 1);
        
        % Store the result
        pupil_avg.(currentField){i} = averageResult;
    end
end

%% Visualize Results (Plotting first 7000 points)
% 4-color palette matching the previous plots
colors = { ...
    [0.1 0.1 0.1],  ... % 1. Gray (Sham + No Reward)
    [0.2 0.6 0.86], ... % 2. Blue (Sham + Reward)
    [0 0.7 0],      ... % 3. Green (tVNS + No Reward)
    [0.9 0.3 0.24]  ... % 4. Red (tVNS + Reward)
};

figure('Name', strcat('Pupil Traces - Subject: ', pat), 'Color', 'w', 'Position', [200 200 800 500]);
hold on;
grid on;
box on;

for i = 1:4
    % Ensure we don't index beyond the array length if it's shorter than 7000
    trace_data = pupil_avg.pupil_size_normalized{i};
    
    if ~isempty(trace_data)
        plot_len = min(7000, length(trace_data));
        
        % Match linestyle format: bold solid lines
        plot(1:plot_len, trace_data(1:plot_len), 'Color', colors{i}, 'LineWidth', 2.5, ...
             'DisplayName', pupil_avg.cond_name{i});
    end
end

title(sprintf('Raw Pupil Size Traces (First 7000 pts) - Subject %s', pat), 'FontSize', 14);
xlabel('Time (samples)', 'FontSize', 12, 'FontWeight', 'bold');
ylabel('Pupil Size (Arbitrary Units)', 'FontSize', 12, 'FontWeight', 'bold');
legend('Location', 'best', 'FontSize', 10);
set(gca, 'FontSize', 11);
hold off;