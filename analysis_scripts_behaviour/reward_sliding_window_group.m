%% 1. Setup
participants = {'FRTP01', 'FRTP02', 'FRTP03', 'FRTP04', 'FRTP05', 'FRTP06', ...
                'FRTP07', 'FRTP09', 'FRTP11', 'FRTP12', ...
                'FRTP14', 'FRTP16', 'FRTP17', 'FRTP18', 'FRTP20', 'FRTP21',  ...
                'FRTP22','FRTP23', 'FRTP24', 'FRTP25', 'FRTP26', 'FRTP27', 'FRTP28', ...
                'FRTP29', 'FRTP30', 'FRTP31', 'FRTP32', 'FRTP33'};
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\Reward_experiment'; 
kinarm_delay = 0.0;

% Parameters matching your function
window_width = 0.0500;
step_size = 0.002;
time_axis = 0:step_size:0.6;

% Pre-allocate matrices (Participants x Time)
all_noreward = nan(length(participants), length(time_axis));
all_reward   = nan(length(participants), length(time_axis));

fprintf('Calculating Grand Average Sliding Window for %d participants...\n', length(participants));

%% 2. Loop Through Participants
for i = 1:length(participants)
    pat = participants{i};
    fprintf('Processing %s... ', pat);
    
    % Load Data
    pat_file = fullfile(base_dir, pat, [pat '_reward_reach_data_analyzed.mat']);
    if ~exist(pat_file, 'file'); fprintf('Skipped (File not found).\n'); continue; end
    L = load(pat_file);
    data = L.reward_data;
    
    % Filter and Unit Correction
    valid_idx = (data.catch_trial == 1);
    RT = (data.eff_pt(valid_idx) / 1000) - kinarm_delay;
    hit = data.success(valid_idx);
    cond = data.reward_type(valid_idx);
    
    % Sliding Window Logic (Per Participant)
    for j = 1:length(time_axis)
        t = time_axis(j);
        win_min = t - (window_width/2);
        win_max = t + (window_width/2);
        
        % No-Reward (Condition 3)
        idx_no = (cond == 3) & (RT >= win_min) & (RT <= win_max);
        if sum(idx_no) > 3
            all_noreward(i, j) = mean(hit(idx_no));
        end
        
        % Reward (Condition 4)
        idx_rew = (cond == 4) & (RT >= win_min) & (RT <= win_max);
        if sum(idx_rew) > 3
            all_reward(i, j) = mean(hit(idx_rew));
        end
    end
    fprintf('Done.\n');
end

%% 3. Calculate Group Statistics
% Hierarchical mean (Average of the individual participant averages)
group_mean_no  = mean(all_noreward, 1, 'omitnan');
group_mean_rew = mean(all_reward, 1, 'omitnan');

% SEM (Variation between participants)
group_sem_no  = std(all_noreward, 0, 1, 'omitnan') ./ sqrt(length(participants));
group_sem_rew = std(all_reward, 0, 1, 'omitnan') ./ sqrt(length(participants));

%% 4. Plotting the Grand Average
figure('Color', 'w', 'Name', 'Grand Average: Sliding Window Accuracy');
hold on; grid on; box on;

% Define plotting helper for shaded SEM
fill_err = @(x, y, err, col) patch([x, fliplr(x)], [(y + err), fliplr(y - err)], col, ...
          'FaceAlpha', 0.2, 'EdgeColor', 'none', 'HandleVisibility', 'off');

% Plot No-Reward (Gray)
valid_no = ~isnan(group_mean_no);
fill_err(time_axis(valid_no), group_mean_no(valid_no), group_sem_no(valid_no), [0.4 0.4 0.4]);
h1 = plot(time_axis(valid_no), group_mean_no(valid_no), 'Color', [0.2 0.2 0.2], 'LineWidth', 3);

% Plot Reward (Red)
valid_rew = ~isnan(group_mean_rew);
fill_err(time_axis(valid_rew), group_mean_rew(valid_rew), group_sem_rew(valid_rew), [0.85 0.33 0.10]);
h2 = plot(time_axis(valid_rew), group_mean_rew(valid_rew), 'Color', [0.64 0.08 0.08], 'LineWidth', 3);

% Individual subject traces (Optional: adds the "spaghetti" look)
plot(time_axis, all_noreward, 'Color', [0.2 0.2 0.2 0.1], 'LineWidth', 0.5, 'HandleVisibility', 'off');
plot(time_axis, all_reward, 'Color', [0.64 0.08 0.08 0.1], 'LineWidth', 0.5, 'HandleVisibility', 'off');

xlabel('Reaction Time (s)', 'FontSize', 12);
ylabel('Hit Rate (Group Mean ± SEM)', 'FontSize', 12);
title(['Grand Average Speed-Accuracy Trade-off (N=' num2str(length(participants)) ')'], 'FontSize', 14);
legend([h1, h2], {'No Reward', 'Reward'}, 'Location', 'southeast');
ylim([0.2 1.0]); xlim([0 0.55]);