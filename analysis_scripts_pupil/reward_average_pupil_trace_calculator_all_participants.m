%% Define Directory and Participants
participants = {'FRTP01', 'FRTP02', 'FRTP03', 'FRTP04', 'FRTP05', 'FRTP06', ...
                'FRTP07', 'FRTP09', 'FRTP11', 'FRTP12', ...
                'FRTP14', 'FRTP16', 'FRTP17', 'FRTP18', 'FRTP20', 'FRTP21',  ...
                'FRTP22','FRTP23', 'FRTP24', 'FRTP25', 'FRTP26', 'FRTP27', 'FRTP28', ...
                'FRTP29', 'FRTP30', 'FRTP31', 'FRTP32', 'FRTP33'};
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\Reward_experiment\'; 

% Pre-allocate a cell array to store each subject's average table
all_subjects_averages = cell(length(participants), 1);

%% Outer Loop: Iterate through each participant
for p = 1:length(participants)
    pat = participants{p};
    fprintf('Processing Subject: %s (%d of %d)\n', pat, p, length(participants));
    
    pat_dir = fullfile(base_dir, pat);
    reward_file = fullfile(pat_dir, [pat, '_reward_reach_data_compact.mat']);
    
    % Check if file exists before loading
    if ~exist(reward_file, 'file')
        warning('File not found for %s, skipping...', pat);
        continue;
    end
    
    load(reward_file); % Loads Compact_all
    reward_data = Compact_all;

    %% Make table to store average values for THIS subject
    pupil_avg = table();
    pupil_avg.SID(1:2) = string(pat);
    pupil_avg.reward_condition(1) = 3;
    pupil_avg.reward_condition(2) = 4;

    %% Inner Loop: Split by reward condition
    for i = 1:height(pupil_avg)
        matchIdx = reward_data.reward_type == pupil_avg.reward_condition(i) & reward_data.pupil_nan_percent < 0.3;
        
        % Data types to process
        fields = {'pupil_size_normalized', 'pupil_size'};
        
        for f = 1:length(fields)
            currentField = fields{f};
            cellArrays = reward_data.(currentField)(matchIdx);
            
            % Clean empty/NaN cells
            nonEmptyCells = cellfun(@(x) x', cellArrays(~cellfun(@isempty, cellArrays) & ...
                                       ~cellfun(@(x) all(isnan(x)), cellArrays)), ...
                                       'UniformOutput', false);
            
            if isempty(nonEmptyCells), continue; end
            
            numericData = cell2mat(nonEmptyCells);
            trialMeans = mean(numericData, 2);
            outlierIdx = isoutlier(trialMeans); 
            
            % Remove outliers and average
            numericData = numericData(~outlierIdx, :);
            pupil_avg.(currentField){i} = mean(numericData, 1);
        end
    end
    
    % Store this subject's result in our master list
    all_subjects_averages{p} = pupil_avg;
end

%% Create Super Grand Average
master_table = vertcat(all_subjects_averages{:});
save(strcat(base_dir, 'pupil_grand_average_reward_all_pats.mat'),"master_table");
%% save in letswave format
table_no_reward = master_table(master_table.reward_condition == 3,:);
table_reward = master_table(master_table.reward_condition == 4,:);

no_reward_mat = zeros(height(table_no_reward), 8001);
reward_mat = zeros(height(table_reward), 8001);

for a = 1:height(no_reward_mat)
    pupil = table_no_reward.pupil_size_normalized{a};
    no_reward_mat(a,:) = pupil;

end

for a = 1:height(reward_mat)
    pupil = table_reward.pupil_size_normalized{a};
    reward_mat(a,:) = pupil;

end


save(strcat(base_dir, '\', "letswave_format_pupil_data.mat"), "no_reward_mat", "reward_mat")


%%
super_grand_avg = table();
conditions = [1, 2];

for c = 1:length(conditions)
    cond_rows = master_table(master_table.reward_condition == c+2, :);
    
    % Stack all subject averages (Rows = Subjects, Cols = Time)
    norm_matrix = cell2mat(cond_rows.pupil_size_normalized);
    raw_matrix = cell2mat(cond_rows.pupil_size);
    
    n_subs = size(norm_matrix, 1); % Number of subjects for SEM
    
    super_grand_avg.reward_condition(c) = conditions(c);
    
    % Calculate Mean
    super_grand_avg.pupil_size_normalized{c} = mean(norm_matrix, 1);
    super_grand_avg.pupil_size{c} = mean(raw_matrix, 1);
    % Calculate SEM: SD / sqrt(n)
    super_grand_avg.sem_normalized{c} = std(norm_matrix, 0, 1) ./ sqrt(n_subs);
    super_grand_avg.sem{c} = std(raw_matrix, 0, 1) ./ sqrt(n_subs);
end

%% Plotting the Super Grand Average with Shaded SEM & Events
figure;
hold on;

% 1. Define Time Vector: Start at -2, 8000 samples total
% Assuming 1000 Hz sampling rate
fs = 1000; 
time_seconds = ((0:7999) / fs) - 2; 

colors = {'b', 'r'}; 
% Define event indices and their labels
events = [2000, 4000, 5500];
labels = {'Precue onset', 'Prep tone 1', 'Prep tone 4'};

% Plot Shaded Area and Mean Lines
for c = 1:2
    % Extract data
    mu = super_grand_avg.pupil_size_normalized{c}(1:8000);
    sem = super_grand_avg.sem_normalized{c}(1:8000);
    
    % Create coordinates for the shaded polygon using the NEW time_seconds
    x_fill = [time_seconds, fliplr(time_seconds)];
    y_fill = [(mu + sem), fliplr(mu - sem)];
    
    % Plot the shaded area
    fill(x_fill, y_fill, colors{c}, 'FaceAlpha', 0.2, 'EdgeColor', 'none', 'HandleVisibility', 'off');
    
    % Plot the mean line
    lineStyle = '-'; if c == 2, lineStyle = '--'; end
    if c ==1
        plot(time_seconds, mu, lineStyle, 'Color', colors{c}, 'LineWidth', 2, 'DisplayName', 'No Reward');
    else
        plot(time_seconds, mu, lineStyle, 'Color', colors{c}, 'LineWidth', 2, 'DisplayName', 'Reward');
    end
end

% 2. Add Vertical Event Lines and Vertical Text
yl = ylim; % Get current y-axis limits for text placement
for e = 1:length(events)
    % Convert sample index to the corresponding time in seconds
    event_time = time_seconds(events(e));
    
    % Draw the vertical line
    line([event_time event_time], yl, 'Color', [0.5 0.5 0.5], 'HandleVisibility', 'off');
    
    % Add text: rotated 90 degrees
    % 'VerticalAlignment', 'bottom' makes it sit to the right of the line when rotated
    text(event_time, yl(2), labels{e}, 'Rotation', 90, 'VerticalAlignment', 'bottom', ...
        'HorizontalAlignment', 'right', 'FontSize', 10, 'Color', [0.3 0.3 0.3]);
end

title('Reward Experiment: Pupil Size Grand Average');
xlabel('Time (seconds)');
ylabel('Normalized Pupil Size');
legend('show', 'Location', 'best');
grid on;
hold off;

%% Plotting the Super Grand Average (raw) with Shaded SEM & Events
figure;
hold on;

% 1. Define Time Vector: Start at -2, 8000 samples total
% Assuming 1000 Hz sampling rate
fs = 1000; 
time_seconds = ((0:7999) / fs) - 2; 

colors = {'b', 'r'}; 
% Define event indices and their labels
events = [2000, 4000, 5500];
labels = {'Precue onset', 'Prep tone 1', 'Prep tone 4'};

% Plot Shaded Area and Mean Lines
for c = 1:2
    % Extract data
    mu = super_grand_avg.pupil_size{c}(1:8000);
    sem = super_grand_avg.sem{c}(1:8000);
    
    % Create coordinates for the shaded polygon using the NEW time_seconds
    x_fill = [time_seconds, fliplr(time_seconds)];
    y_fill = [(mu + sem), fliplr(mu - sem)];
    
    % Plot the shaded area
    fill(x_fill, y_fill, colors{c}, 'FaceAlpha', 0.2, 'EdgeColor', 'none', 'HandleVisibility', 'off');
    
    % Plot the mean line
    lineStyle = '-'; if c == 2, lineStyle = '--'; end
    plot(time_seconds, mu, lineStyle, 'Color', colors{c}, 'LineWidth', 2, 'DisplayName', sprintf('Condition %d', c));
end

% 2. Add Vertical Event Lines and Vertical Text
yl = ylim; % Get current y-axis limits for text placement
for e = 1:length(events)
    % Convert sample index to the corresponding time in seconds
    event_time = time_seconds(events(e));
    
    % Draw the vertical line
    line([event_time event_time], yl, 'Color', [0.5 0.5 0.5], 'HandleVisibility', 'off');
    
    % Add text: rotated 90 degrees
    % 'VerticalAlignment', 'bottom' makes it sit to the right of the line when rotated
    text(event_time, yl(2), labels{e}, 'Rotation', 90, 'VerticalAlignment', 'bottom', ...
        'HorizontalAlignment', 'right', 'FontSize', 10, 'Color', [0.3 0.3 0.3]);
end

title('Reward Experiment: Super Grand Average with SEM');
xlabel('Time (seconds)');
ylabel('Normalized Pupil Size');
legend('show', 'Location', 'best');
grid on;
hold off;