%% Hierarchical Group Analysis: tVNS Experiment
participants = {'FRTP01', 'FRTP02', 'FRTP03', 'FRTP04', 'FRTP05', 'FRTP06', 'FRTP07', 'FRTP09', 'FRTP12', 'FRTP14', 'FRTP16', 'ALAN', 'LEA'}; 
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_Data\tVNS_experiment\';

% Pre-allocate: 8 parameters [mu, s, qI, qA, D_mu, D_qA, D_s, D_qI]
group_params = zeros(length(participants), 9); 

fprintf('Processing tVNS Hierarchical Fit (%d participants)...\n', length(participants));
for i = 1:length(participants)
    pat = participants{i};
    fprintf('Fitting Participant: %s... ', pat);
    
    % 1. Load Data
    pat_dir = fullfile(base_dir, pat);
    load(fullfile(pat_dir, [pat '_tvns_reach_data_analyzed.mat'])); % Adjust naming if needed
    tvns_data = tvns_data_light;
    % 2. Extract Variables
    % Note: Ensure RT is in seconds and stim_condition uses 1=Sham, 2=Active
    rt = (tvns_data.eff_pt / 1000)-0.02; 
    hit = tvns_data.success;
    cond = tvns_data.stim_type; 
    
    % 1. Fit the 8-parameter model
    model = tvns_fit_8param_model(rt, hit, cond);
    p = model.paramsOpt;
    
    % 2. CALCULATE EFFICIENCY METRIC (RT required for 75% relative performance)
    % This captures if the curve "rises" faster due to either mu or sigma changes.
    % We solve for RT where: normcdf(RT, mu, sigma) = 0.5 (the midpoint)
    % or more comprehensively, we look at the change in "effective speed"
    
    % Efficiency = mu + sigma (A simple heuristic for 'Slow and Noisy')
    % Change in Efficiency = Delta_mu + Delta_sigma
    eff_sham = p(1) + p(2);
    eff_stim = (p(1)+p(5)) + (p(2)+p(7));
    delta_eff = eff_stim - eff_sham; 
    
    % Store: [Original 8 params, delta_efficiency]
    group_params(i, :) = [p, delta_eff];
end

%% --- PART 2: Updated Forest Plot ---
% We now have 5 "Delta" metrics to plot
delta_metrics = group_params(:, 5:9);
delta_names = {'Delta mu', 'Delta qA', 'Delta sig', 'Delta qI', 'Efficiency Shift'};

mean_deltas = mean(delta_metrics, 1);
sem_deltas = std(delta_metrics, 0, 1) / sqrt(length(participants));

figure('Name', 'tVNS Hierarchical Forest Plot', 'Color', 'w');
% Plot the first 4 (Original Deltas)
errorbar(mean_deltas(1:4), 1:4, sem_deltas(1:4)*1.96, 'ko', 'Horizontal', 'LineWidth', 2);
hold on;

% Plot the Efficiency Metric in a different color to highlight it
errorbar(mean_deltas(5), 5, sem_deltas(5)*1.96, 'ro', 'Horizontal', 'LineWidth', 2, 'DisplayName', 'Composite Efficiency');

set(gca, 'YTick', 1:5, 'YTickLabel', delta_names, 'YDir', 'reverse');
line([0 0], [0 6], 'Color', [0.5 0.5 0.5], 'LineStyle', '--'); % Zero line
xlabel('Change (Active - Sham)');
title('Group tVNS Effects: Standard vs Composite Metrics');
grid on;

%% --- PART 3: Significance Summary ---
p_vals = zeros(1, 5);
for j = 1:5
    [~, p_vals(j)] = ttest(delta_metrics(:,j), 0);
end

stats_table = table(delta_names', mean_deltas', p_vals', ...
    'VariableNames', {'Metric', 'Mean_Change', 'P_Value'});
disp(stats_table);

%% --- PART 4: Psychometric Visualization ---
figure('Name', 'tVNS Hierarchical Curves', 'Color', 'w');
hold on;
t_plot = 0:0.001:0.5;

% Plot Individual Subject "Spaghetti" Lines
for i = 1:length(participants)
    p = group_params(i, :);
    % Sham
    y_sham = p(3) + (p(4) - p(3)) * normcdf(t_plot, p(1), p(2));
    plot(t_plot, y_sham, 'Color', [0.8 0.8 0.8], 'HandleVisibility', 'off');
    % Active
    y_stim = (p(3)+p(8)) + ((p(4)+p(6)) - (p(3)+p(8))) * normcdf(t_plot, p(1)+p(5), p(2)+p(7));
    plot(t_plot, y_stim, 'Color', [0.7 0.9 0.7], 'HandleVisibility', 'off');
end

% Plot Bold Group Means
gp = mean(group_params, 1);
y_sham_gp = gp(3) + (gp(4) - gp(3)) * normcdf(t_plot, gp(1), gp(2));
y_stim_gp = (gp(3)+gp(8)) + ((gp(4)+gp(6)) - (gp(3)+gp(8))) * normcdf(t_plot, gp(1)+gp(5), gp(2)+gp(7));

plot(t_plot, y_sham_gp, 'k-', 'LineWidth', 2, 'DisplayName', 'Group Sham');
plot(t_plot, y_stim_gp, 'g-', 'LineWidth', 2, 'DisplayName', 'Group Active (tVNS)');

xlabel('Reaction Time (s)'); ylabel('Accuracy');
title('Hierarchical Group Trend: tVNS Effect');
legend('Location', 'southeast');
grid on;

%%
% Is the early rise (D_mu) significant across the group?
[h, p_val] = ttest(group_params(:, 5), 0); 
fprintf('Significance of Reward on Mu: p = %.4f\n', p_val);

[h, p_val] = ttest(group_params(:, 7), 0); 
fprintf('Significance of Reward on Sigma: p = %.4f\n', p_val);

[h, p_val] = ttest(group_params(:, 6), 0); 
fprintf('Significance of Reward on qA: p = %.4f\n', p_val);

% Is the change in baseline (D_qI) significant?
[h, p_val_qI] = ttest(group_params(:, 8), 0);
fprintf('Significance of Reward on qI: p = %.4f\n', p_val_qI);