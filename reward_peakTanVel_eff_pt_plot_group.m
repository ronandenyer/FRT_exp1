%% Define Reward Study Details
participants = {'FRTP01', 'FRTP02', 'FRTP03', 'FRTP04', 'FRTP06', 'FRTP07', ...
    'FRTP09', 'FRTP11', 'FRTP12', 'FRTP14', 'FRTP15', 'FRTP16', 'FRTP17', ...
    'FRTP18', 'FRTP21', 'FRTP22'}; base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\Reward_experiment'; 

% Parameters
WINDOW_SIZE_POINTS = 100; 
REWARD_NO = 3;
REWARD_YES = 4;
COLOR_NO_REWARD = [0 0.4470 0.7410]; % Blue
COLOR_REWARD = [0.8500 0.3250 0.0980];    % Red/Orange

% Define common time axis (adjust 3000 to your max expected prep_time)
common_time = linspace(0, 500, 1000)'; 

% Pre-allocate matrices [time x participants]
no_reward_matrix = NaN(length(common_time), length(participants));
reward_matrix = NaN(length(common_time), length(participants));

%% 1. Process Participants
for p = 1:length(participants)
    pat = participants{p};
    fprintf('Processing participant: %s...\n', pat);
    
    try
        pat_dir = fullfile(base_dir, pat);
        reward_file = fullfile(pat_dir, sprintf('%s_reward_reach_data_analyzed.mat', pat));
        load(reward_file, 'reward_data');

        pat_file_free = fullfile(base_dir, pat, [pat, '_free_response_reach_data_compact.mat']);
        load(pat_file_free);
    

        filter_outliers = @(data) data(data >= (quantile(data, 0.25) - 1.5*iqr(data)) & ...
                                   data <= (quantile(data, 0.75) + 1.5*iqr(data)));

        peak_tanVel_clean = filter_outliers(Compact.peak_tanVel(~isnan(Compact.peak_tanVel)));

        mean_free_peakVel = mean(peak_tanVel_clean);
        
        % Data Cleaning
        exclude_blocks = [0];
        reward_data = reward_data(~ismember(reward_data.block_num, exclude_blocks),:);
        
        for condition_idx = 1:2
            curr_type = iff(condition_idx == 1, REWARD_NO, REWARD_YES);
            
            % Filter, Remove NaNs, and Sort
            data_sub = reward_data(reward_data.reward_type == curr_type, :);
            data_sub(isnan(data_sub.peak_tanVel) | isnan(data_sub.prep_time), :) = [];
            data_sub = sortrows(data_sub, 'prep_time');
            
            if ~isempty(data_sub)
                % Calculate individual smoothed velocity
                norm_peak_tanVel = data_sub.peak_tanVel/mean_free_peakVel;
                smoothed_vel = movmean(norm_peak_tanVel, WINDOW_SIZE_POINTS);
                
                % Interpolate onto common axis
                [unique_x, idx] = unique(data_sub.prep_time);
                interp_vel = interp1(unique_x, smoothed_vel(idx), common_time, 'linear', NaN);
                
                if condition_idx == 1
                    no_reward_matrix(:, p) = interp_vel;
                else
                    reward_matrix(:, p) = interp_vel;
                end
            end
        end
    catch ME
        fprintf('Warning: Could not process %s. Error: %s\n', pat, ME.message);
    end
end

%% 2. Grand Average Calculations (Between-Subject)
ga_no = nanmean(no_reward_matrix, 2);
ga_yes = nanmean(reward_matrix, 2);

sem_no = nanstd(no_reward_matrix, 0, 2) ./ sqrt(sum(~isnan(no_reward_matrix), 2));
sem_yes = nanstd(reward_matrix, 0, 2) ./ sqrt(sum(~isnan(reward_matrix), 2));

%% 3. Plotting
figure('Color', 'w', 'Name', 'Reward Grand Average');
hold on; grid on; box on;

% 3a. Plot Individual Faint Traces
for p = 1:length(participants)
    plot(common_time, no_reward_matrix(:,p), 'Color', [COLOR_NO_REWARD, 0.12], ...
        'LineWidth', 0.5, 'HandleVisibility', 'off');
    plot(common_time, reward_matrix(:,p), 'Color', [COLOR_REWARD, 0.12], ...
        'LineWidth', 0.5, 'HandleVisibility', 'off');
end

% 3b. Plot Shaded SEM Regions
fill_no_x = [common_time; flipud(common_time)];
fill_no_y = [ga_no + sem_no; flipud(ga_no - sem_no)];
fill(fill_no_x(~isnan(fill_no_y)), fill_no_y(~isnan(fill_no_y)), ...
    COLOR_NO_REWARD, 'FaceAlpha', 0.25, 'EdgeColor', 'none', 'HandleVisibility', 'off');

fill_yes_x = [common_time; flipud(common_time)];
fill_yes_y = [ga_yes + sem_yes; flipud(ga_yes - sem_yes)];
fill(fill_yes_x(~isnan(fill_yes_y)), fill_yes_y(~isnan(fill_yes_y)), ...
    COLOR_REWARD, 'FaceAlpha', 0.25, 'EdgeColor', 'none', 'HandleVisibility', 'off');

% 3c. Plot Bold Grand Average Lines
p1 = plot(common_time, ga_no, 'Color', COLOR_NO_REWARD, 'LineWidth', 3, 'DisplayName', 'No Reward (3)');
p2 = plot(common_time, ga_yes, 'Color', COLOR_REWARD, 'LineWidth', 3, 'DisplayName', 'Reward (4)');

%% 4. Customization
xlabel('Effective Point (eff\_pt, ms)');
ylabel('Peak Tangential Velocity');
title(sprintf('Grand Average (n=%d) Reward Response with Individual Traces', sum(any(~isnan(no_reward_matrix)))));
legend([p1, p2], 'Location', 'best');
xlim([min(common_time) max(common_time)]);

%% Helper Function
function res = iff(condition, true_val, false_val)
    if condition, res = true_val; else, res = false_val; end
end