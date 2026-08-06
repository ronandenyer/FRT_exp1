%% Hierarchical Group Analysis Script
participants = {'FRTP01', 'FRTP02', 'FRTP03', 'FRTP04', 'FRTP05', 'FRTP06', 'FRTP07', 'FRTP09','FRTP11', 'FRTP12', 'FRTP13', 'ALAN', 'LEA', 'RONAN'}; % Add your IDs
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\Reward_experiment';

% Pre-allocate matrix for 8 parameters x N participants
group_params = zeros(length(participants), 8); 

fprintf('Starting Hierarchical Fit for %d participants...\n', length(participants));

for i = 1:length(participants)
    pat = participants{i};
    fprintf('Fitting Participant: %s... ', pat);
    
    % 1. Load Data
    pat_dir = fullfile(base_dir, pat);
    load(fullfile(pat_dir, [pat '_reward_reach_data_analyzed.mat']));
    
    % 2. Clean Data (Reuse your logic)
    valid_idx = (reward_data.catch_trial == 1); 
    rt = (reward_data.eff_pt(valid_idx) / 1000)-0.02; 
    hit = reward_data.success(valid_idx);
    cond = reward_data.reward_type(valid_idx);
    
    % 3. Run the 8-parameter Logistic Model
    model = reward_fit_unified_model_logistic(rt, hit, cond);
    
    % 4. Store Parameters
    group_params(i, :) = model.paramsOpt;
    fprintf('Done.\n');
end

%% --- PART 2: Hierarchical Inference ---
% Calculate the Group Mean (The "Hyper-parameter" Mean)
group_mean_params = mean(group_params, 1);
group_sem_params = std(group_params, 0, 1) / sqrt(length(participants));

% Compute Group Stats for the Deltas (p5 to p8)
deltas = group_params(:, 5:8);
delta_names = {'Delta_mu', 'Delta_qA', 'Delta_sig', 'Delta_qI'};
mean_deltas = mean(deltas, 1);
sem_deltas = std(deltas, 0, 1) / sqrt(length(participants));

% Convert to Table for easy reading
param_names = {'mu_base', 's_base', 'qI_base', 'qA_base', 'D_mu', 'D_qA', 'D_s', 'D_qI'};
group_table = array2table(group_params, 'VariableNames', param_names, 'RowNames', participants);

disp('--- Group Parameter Summary ---');
disp(group_table);

%% --- PART 3: Forest Plot (Visualizing the Group Effect) ---
figure('Name', 'tVNS Effect Size: Forest Plot', 'Color', 'w');
errorbar(mean_deltas, 1:4, [], [], sem_deltas*1.96, sem_deltas*1.96, 'ko', 'LineWidth', 2, 'CapSize', 10);
set(gca, 'YTick', 1:4, 'YTickLabel', delta_names, 'YDir', 'reverse');
line([0 0], [0 5], 'Color', 'r', 'LineStyle', '--'); % Null line
xlabel('Parameter Shift (Reward - No Reward)');
title('Group-Level tVNS Effects (Mean ± 95% CI)');
grid on;
%% --- PART 4: Group Visualization ---
figure('Name', 'Hierarchical Group Effect', 'Color', 'w');
hold on;

t_plot = 0:0.001:0.5;
logistic_func = @(t, m, s) 1 ./ (1 + exp(-(t - m) ./ s));

% Plot individual participants in faint lines
for i = 1:length(participants)
    p = group_params(i, :);
    % Baseline (Faint Blue)
    y0 = p(3) + (p(4)-p(3)) * logistic_func(t_plot, p(1), p(2));
    plot(t_plot, y0, 'Color', [0.8 0.8 1], 'LineWidth', 0.5, 'HandleVisibility', 'off');
    % Reward (Faint Red)
    y1 = (p(3)+p(8)) + ((p(4)+p(6))-(p(3)+p(8))) * logistic_func(t_plot, p(1)+p(5), p(2)+p(7));
    plot(t_plot, y1, 'Color', [1 0.8 0.8], 'LineWidth', 0.5, 'HandleVisibility', 'off');
end

% Plot the Hierarchical Group Mean (Bold)
gp = group_mean_params;
% Group Baseline
y0_group = gp(3) + (gp(4)-gp(3)) * logistic_func(t_plot, gp(1), gp(2));
plot(t_plot, y0_group, 'b-', 'LineWidth', 2, 'DisplayName', 'Group: No-Reward');

% Group Reward
y1_group = (gp(3)+gp(8)) + ((gp(4)+gp(6))-(gp(3)+gp(8))) * logistic_func(t_plot, gp(1)+gp(5), gp(2)+gp(7));
plot(t_plot, y1_group, 'r-', 'LineWidth', 2, 'DisplayName', 'Group: Reward');

% Final Aesthetics
grid on; xlabel('RT (s)'); ylabel('Accuracy');
title('Hierarchical Group Trend: Reward vs No-Reward');
legend('Location', 'southeast');

%%
% Is the early rise (D_mu) significant across the group?
[h, p_val] = ttest(group_params(:, 5), 0); 
fprintf('Significance of Reward on Mu: p = %.4f\n', p_val);

% Is the change in baseline (D_qI) significant?
[h, p_val_qI] = ttest(group_params(:, 7), 0);
fprintf('Significance of Reward on qI: p = %.4f\n', p_val_qI);