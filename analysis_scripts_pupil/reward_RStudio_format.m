%% load data file with average data for each participants

data_dir = "C:\Users\denyer\OneDrive - UCL\FRT_data\Reward_experiment\";

filename = 'pupil_grand_average_reward_all_pats.mat';

load(strcat(data_dir, filename));
pupil_avg_global_all = master_table;
%% create grand average table

pupil_grand_avg = table();

pupil_grand_avg.reward_condition(1) = 3;
pupil_grand_avg.reward_condition(2) = 4;

% Loop through each row of pupil_grand_avg
for i = 1:height(pupil_grand_avg)
    % Process each field
    fields = {'pupil_size_normalized', 'pupil_size'};
    
    for j = 1:length(fields)
        fieldName = fields{j};

        % Extract relevant cell arrays
        cellArrays = pupil_avg_global_all.(fieldName)(pupil_avg_global_all.reward_condition == pupil_grand_avg.reward_condition(i));

        % Convert to numeric matrix
        numericData = cell2mat(cellArrays)';

        % Debugging check
        disp(['Field: ', fieldName, ', Size: ', mat2str(size(numericData))]);

        % Ensure correct dimensions (should be 6000 × num_trials)
        if isempty(numericData) || size(numericData, 1) ~= 8001
            warning(['Skipping ', fieldName, ' due to incorrect dimensions: ', mat2str(size(numericData))]);
            continue;
        end

        % Compute mean across trials
        meanResult = mean(numericData, 2);

        % Standard error of the mean (SEM)
        semResult = std(numericData, 0, 2) / sqrt(size(numericData, 2));

        % Compute SEM plus and minus
        semLower = meanResult - semResult;
        semUpper = meanResult + semResult;

        % Store in table
        pupil_grand_avg.(fieldName){i} = meanResult;
        pupil_grand_avg.([fieldName '_sem_lower']){i} = semLower;
        pupil_grand_avg.([fieldName '_sem_upper']){i} = semUpper;
    end
end

ggplot_format_pupil_reward = table();

ggplot_format_pupil_reward.time = (1:8001)'/1000;
ggplot_format_pupil_reward.no_reward_mean = pupil_grand_avg.pupil_size_normalized{1};
ggplot_format_pupil_reward.no_reward_sem_upper = pupil_grand_avg.pupil_size_normalized_sem_upper{1};
ggplot_format_pupil_reward.no_reward_sem_lower = pupil_grand_avg.pupil_size_normalized_sem_lower{1};
ggplot_format_pupil_reward.reward_mean = pupil_grand_avg.pupil_size_normalized{2};
ggplot_format_pupil_reward.reward_sem_upper = pupil_grand_avg.pupil_size_normalized_sem_upper{2};
ggplot_format_pupil_reward.reward_sem_lower = pupil_grand_avg.pupil_size_normalized_sem_lower{2};

writetable(ggplot_format_pupil_reward, strcat(data_dir, "pupil_grand_mean_reward.csv"));