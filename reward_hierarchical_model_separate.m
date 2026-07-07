%% Independent Hierarchical Group Analysis: Reward Experiment
participants = {'FRTP01', 'FRTP02', 'FRTP03', 'FRTP04', 'FRTP05', 'FRTP06', 'FRTP07', 'FRTP09', 'FRTP11', 'FRTP12', 'FRTP14', 'FRTP15', 'FRTP16', 'FRTP17'}; 
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\Reward_experiment';
kinarm_delay = 0.0;

% Pre-allocate: [mu, sigma, qI, qA]
params_noreward = zeros(length(participants), 4);
params_reward   = zeros(length(participants), 4);

fprintf('Starting Independent Fits for %d participants (Reward Exp)...\n', length(participants));

for i = 1:length(participants)
    pat = participants{i};
    fprintf('Fitting Participant: %s... ', pat);
    
    % 1. Load Data
    pat_file = fullfile(base_dir, pat, [pat '_reward_reach_data_analyzed.mat']);
    L = load(pat_file);
    data = L.reward_data;
    data = data(data.catch_trial == 1, :);
    
    % 2. Clean Data (Forced Response: RT is actually Viewing Time)
    valid_idx = (data.catch_trial == 1); 
    VT = (data.eff_pt(valid_idx) / 1000) - kinarm_delay; 
    hit = data.success(valid_idx);
    cond = data.reward_type(valid_idx); % 3 = No-Reward, 4 = Reward
    
    % 3. Run model for No-Reward (Condition 3)
    idx_no = (cond == 3);
    m_no = fit_model(VT(idx_no), hit(idx_no));
    params_noreward(i, :) = m_no.paramsOpt;
    
    % 4. Run model for Reward (Condition 4)
    idx_rew = (cond == 4);
    m_rew = fit_model(VT(idx_rew), hit(idx_rew));
    params_reward(i, :) = m_rew.paramsOpt;
    
    fprintf('Done.\n');
end

%% --- PART 2: Hierarchical Deltas & Efficiency ---
% Delta = Reward - No-Reward
raw_deltas = params_reward - params_noreward;

% Efficiency Metric: Positive = Better (Faster/Steeper/Higher)
% We flip Mu (since lower mu is better) and use (mu + sigma)
eff_no  = params_noreward(:,1) + params_noreward(:,2);
eff_rew = params_reward(:,1) + params_reward(:,2);
delta_eff = (eff_no - eff_rew) * 1000; % Convert to ms gain

% Compile Forest Plot Metrics
% 1: Speed Gain (ms), 2: Noise Change (ms), 3: Accuracy Gain (%), 4: Efficiency (ms)
forest_data = [raw_deltas(:,1)*-1000, raw_deltas(:,2)*1000, raw_deltas(:,4)*100, delta_eff];
metric_labels = {'Speed Gain (ms)', 'Noise Increase (ms)', 'Accuracy Gain (%)', 'Overall Efficiency (ms)'};
all_deltas = [raw_deltas, delta_eff];
delta_names = {'\Delta \mu (Shift)', '\Delta \sigma (Slope)', '\Delta q_I (Floor)', '\Delta q_A (Ceiling)', 'Efficiency Shift'};

%% --- PART 2.5: Statistical Inference ---
p_vals = zeros(1, 5);
for j = 1:5
    [~, p_vals(j)] = ttest(all_deltas(:,j), 0);
end

stats_table = table(delta_names', mean(all_deltas)', p_vals', ...
    'VariableNames', {'Metric', 'Mean_Delta', 'P_Value'});
disp('--- Independent Fit: Group Results ---');
disp(stats_table);

%% --- PART 3: Improved Forest Plot ---
figure('Color', 'w', 'Name', 'Reward Forest Plot', 'Position', [100 100 900 500]);
t = tiledlayout(1, 4, 'TileSpacing', 'compact');

for j = 1:4
    nexttile;
    data = forest_data(:,j);
    mu_val = mean(data);
    sem_val = std(data)/sqrt(length(data));
    
    % Individual participants (Gray dots)
    plot(data, 1:length(data), 'ko', 'MarkerFaceColor', [.8 .8 .8]); hold on;
    
    % Group Mean (Red Square) with 95% CI
    errorbar(mu_val, length(data)+1.5, sem_val*2, 'horizontal', 'rs', 'LineWidth', 2, 'MarkerSize', 8, 'MarkerFaceColor', 'r');
    
    title(metric_labels{j}); grid on;
    set(gca, 'YTick', 1:length(data)+2, 'YTickLabel', [participants, {' ', 'GROUP'}]);
    line([0 0], [0 length(data)+2], 'Color', 'k', 'LineStyle', '--');
    if j == 1; ylabel('Participants'); end
    axis tight;
end
sgtitle('Reward-Induced Shifts in Decision Speed-Accuracy Tradeoff Curve', 'FontSize', 15);

%% --- PART 4: Grand Average Visualization ---
figure('Name', 'Grand Average: Reward vs No-Reward', 'Color', 'w');
hold on; grid on;
t_plot = 0:0.001:0.5;

% Calculate Grand Average Curves
gp_no = mean(params_noreward, 1);
gp_rew = mean(params_reward, 1);

% Plot Function
get_y = @(p) p(3) + (p(4)-p(3)) * normcdf(t_plot, p(1), p(2));

% Plot Bold Mean Curves
plot(t_plot, get_y(gp_no), 'k-', 'LineWidth', 2, 'DisplayName', 'No-Reward');
plot(t_plot, get_y(gp_rew), 'r-', 'LineWidth', 2, 'DisplayName', 'Reward');

% Plot Faint Individual Spaghetti Lines
for i = 1:length(participants)
    plot(t_plot, get_y(params_noreward(i,:)), 'k:', 'LineWidth', 0.5, 'HandleVisibility', 'off');
    plot(t_plot, get_y(params_reward(i,:)), 'r:', 'LineWidth', 0.5, 'HandleVisibility', 'off', 'Color', [1 0.4 0.4 0.3]);
end

xlabel('Preparation Time (s)'); ylabel('Hit Probability');
title('Reward-Induced Shifts in Decision Speed-Accuracy Tradeoff Curve (N=9)');
legend('Location', 'southeast'); ylim([0 1]); xlim([0 0.5]);