%% 1. Setup
participants = {'FRTP01', 'FRTP02', 'FRTP03', 'FRTP04', 'FRTP05', 'FRTP06', ...
                'FRTP07', 'FRTP09', 'FRTP11', 'FRTP12', 'FRTP14', 'FRTP16', ...
                'FRTP17', 'FRTP18', 'FRTP20', 'FRTP21', 'FRTP22', 'FRTP23', 'FRTP24', ...
                'FRTP25', 'FRTP26', 'FRTP27', 'FRTP28', 'FRTP30', 'FRTP31'};
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\tvns_experiment'; 
kinarm_delay = 0.02;

% Parameters matching your sliding window function
window_width = 0.100; % 100ms
step_size = 0.002;    % 2ms steps
time_axis = 0:step_size:0.6;

% Pre-allocate Matrices (Participants x Time)
all_sham = nan(length(participants), length(time_axis));
all_tvns = nan(length(participants), length(time_axis));

fprintf('Calculating Grand Average for tVNS (%d participants)...\n', length(participants));

%% 2. Loop Through Participants
for i = 1:length(participants)
    pat = participants{i};
    fprintf('Processing %s... ', pat);
    
    % Load Data
    pat_file = fullfile(base_dir, pat, [pat '_tvns_reach_data_analyzed.mat']);
    if ~exist(pat_file, 'file'); fprintf('Skipped (Not found).\n'); continue; end
    
    L = load(pat_file);
    % Map to your specific variable name
    data = L.tvns_data_light; 
    
    % Data Preparation
    RT = (data.eff_pt / 1000) - kinarm_delay;
    hit = data.success;
    % Assuming 1 = Sham, 2 = Active tVNS (match your tvns_fit_8param_model logic)
    cond = data.stim_type; 
    
    % Sliding Window Logic (Per Participant)
    for j = 1:length(time_axis)
        t = time_axis(j);
        win_min = t - (window_width/2);
        win_max = t + (window_width/2);
        
        % Sham Condition (Condition 1)
        idx_sham = (cond == 1) & (RT >= win_min) & (RT <= win_max);
        if sum(idx_sham) > 3
            all_sham(i, j) = mean(hit(idx_sham));
        end
        
        % tVNS Condition (Condition 2)
        idx_tvns = (cond == 2) & (RT >= win_min) & (RT <= win_max);
        if sum(idx_tvns) > 3
            all_tvns(i, j) = mean(hit(idx_tvns));
        end
    end
    fprintf('Done.\n');
end

%% 3. Calculate Group Statistics (Mean and SEM)
group_mean_sham = mean(all_sham, 1, 'omitnan');
group_mean_tvns = mean(all_tvns, 1, 'omitnan');

group_sem_sham = std(all_sham, 0, 1, 'omitnan') ./ sqrt(length(participants));
group_sem_tvns = std(all_tvns, 0, 1, 'omitnan') ./ sqrt(length(participants));

%% 4. Plotting the Grand Average
figure('Color', 'w', 'Name', 'tVNS Grand Average Sliding Window');
hold on; grid on; box on;

% Plot Helper for Shaded SEM
fill_err = @(x, y, err, col) patch([x, fliplr(x)], [(y + err), fliplr(y - err)], col, ...
          'FaceAlpha', 0.15, 'EdgeColor', 'none', 'HandleVisibility', 'off');

% Plot Sham (Gray/Black)
valid_s = ~isnan(group_mean_sham);
fill_err(time_axis(valid_s), group_mean_sham(valid_s), group_sem_sham(valid_s), [0.4 0.4 0.4]);
h1 = plot(time_axis(valid_s), group_mean_sham(valid_s), 'k-', 'LineWidth', 3);

% Plot tVNS (Green)
valid_t = ~isnan(group_mean_tvns);
fill_err(time_axis(valid_t), group_mean_tvns(valid_t), group_sem_tvns(valid_t), [0.2 0.8 0.2]);
h2 = plot(time_axis(valid_t), group_mean_tvns(valid_t), 'Color', [0.1 0.6 0.1], 'LineWidth', 3);

% Individual "Spaghetti" Traces for tVNS condition (Visualizing variability)
plot(time_axis, all_tvns, 'Color', [0.1 0.6 0.1 0.1], 'LineWidth', 0.5, 'HandleVisibility', 'off');

xlabel('Reaction Time (s)', 'FontSize', 12);
ylabel('Hit Rate (Proportion)', 'FontSize', 12);
title(['tVNS Grand Average Accuracy (N=' num2str(length(participants)) ')'], 'FontSize', 14);
legend([h1, h2], {'Sham', 'tVNS (Active)'}, 'Location', 'southeast');
ylim([0.2 1.0]); xlim([0 0.55]);

set(gca, 'FontSize', 11);