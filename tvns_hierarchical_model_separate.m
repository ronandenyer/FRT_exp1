%% Independent Hierarchical Group Analysis: tVNS Experiment
%participants = {'FRTP01', 'FRTP02', 'FRTP03', 'FRTP04', 'FRTP05', 'FRTP06', 'FRTP07', 'FRTP09', 'FRTP12', 'FRTP14', 'FRTP16'}; 
participants = {'FRTP03', 'FRTP04', 'FRTP14', 'FRTP17', 'FRTP18', 'FRTP22', 'FRTP23', 'FRTP24', 'FRTP26', 'FRTP27', 'FRTP29', 'FRTP30', 'FRTP31', 'FRTP32'};
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\tvns_experiment'; 
kinarm_delay = 0.0;

% Store parameters: [mu, sigma, qI, qA]
params_sham = zeros(length(participants), 4);
params_active = zeros(length(participants), 4);

fprintf('Running Independent Fits for %d participants...\n', length(participants));

for i = 1:length(participants)
    pat = participants{i};
    fprintf('Processing %s... ', pat);
    
    % 1. Load Data
    pat_file = fullfile(base_dir, pat, [pat '_tvns_reach_data_analyzed.mat']);
    L = load(pat_file);
    data = L.tvns_data_light;
    data = data(data.catch_trial == 1,:);
    
    % 2. Clean and Prepare
    RT = (data.eff_pt / 1000) - kinarm_delay;
    hit = data.success;
    cond = data.stim_type; % 1 = Sham, 2 = Active
    
    % 3. Run model for Sham separately
    idx_s = (cond == 1);
    m_sham = fit_model(RT(idx_s), hit(idx_s));
    params_sham(i, :) = m_sham.paramsOpt;
    
    % 4. Run model for Active separately
    idx_a = (cond == 2);
    m_active = fit_model(RT(idx_a), hit(idx_a));
    params_active(i, :) = m_active.paramsOpt;
    
    fprintf('Done.\n');
end

%% --- PART 2: Calculate Hierarchical Deltas ---
% Delta = Active - Sham
group_deltas = params_active - params_sham;

% Calculate an "Efficiency Shift" based on our previous discussion
% Using (mu + sigma) as the cost metric
eff_sham = params_sham(:,1) + params_sham(:,2);
eff_active = params_active(:,1) + params_active(:,2);
delta_eff = eff_active - eff_sham;

% Combine for Forest Plot
all_deltas = [group_deltas, delta_eff];
delta_names = {'\Delta \mu (Shift)', '\Delta \sigma (Slope)', '\Delta q_I (Floor)', '\Delta q_A (Ceiling)', 'Efficiency Shift'};

%% --- PART 3: Statistical Inference ---
p_vals = zeros(1, 5);
for j = 1:5
    [~, p_vals(j)] = ttest(all_deltas(:,j), 0);
end

stats_table = table(delta_names', mean(all_deltas)', p_vals', ...
    'VariableNames', {'Metric', 'Mean_Delta', 'P_Value'});
disp('--- Independent Fit: Group Results ---');
disp(stats_table);

%% --- PART 4: The "Global Shift" Visualization ---
figure('Color', 'w', 'Name', 'Independent Fit: Grand Average Curves');
hold on; grid on;
t_plot = 0:0.001:0.5;

% Plot Grand Average Curves
gp_s = mean(params_sham, 1);
gp_a = mean(params_active, 1);

% Grand Average Sham
y_s = gp_s(3) + (gp_s(4)-gp_s(3))*normcdf(t_plot, gp_s(1), gp_s(2));
plot(t_plot, y_s, 'k-', 'LineWidth', 2, 'DisplayName', 'Sham');

% Grand Average Active
y_a = gp_a(3) + (gp_a(4)-gp_a(3))*normcdf(t_plot, gp_a(1), gp_a(2));
plot(t_plot, y_a, 'Color', [0.1 0.7 0.1], 'LineWidth', 2, 'DisplayName', 'tVNS');

% Overlay individual subject "Deltas" as faint lines
for i = 1:length(participants)
    p_s = params_sham(i,:);
    p_a = params_active(i,:);
    y_si = p_s(3) + (p_s(4)-p_s(3))*normcdf(t_plot, p_s(1), p_s(2));
    y_ai = p_a(3) + (p_a(4)-p_a(3))*normcdf(t_plot, p_a(1), p_a(2));
    plot(t_plot, y_si, 'k:', 'LineWidth', 0.5, 'HandleVisibility', 'off');
    plot(t_plot, y_ai, 'Color', [0.1 0.7 0.1 0.3], 'LineWidth', 0.5, 'HandleVisibility', 'off');
end

xlabel('Preparation Time (s)'); ylabel('Hit probability');
title('tVNS-Induced Shifts in Decision Speed-Accuracy Tradeoff Curve (N=10)');
legend('Location', 'southeast');
ylim([0 1]); xlim([0 0.5]);

%% --- PART 5: Forest Plot Generation ---
delta_metrics = [group_deltas(:,1)*-1000, ... % Convert Delta Mu to ms and flip (Positive = Faster)
                 group_deltas(:,2)*1000, ...  % Delta Sigma in ms
                 group_deltas(:,4)*100, ...   % Delta qA in Percentage points
                 delta_eff*-1000];            % Efficiency in ms (Positive = Better)

metric_labels = {'Speed Gain (ms)', ...
                 'Noise Increase (ms)', ...
                 'Accuracy Gain (%)', ...
                 'Overall Efficiency (ms)'};

figure('Color', 'w', 'Name', 'tVNS Independent Fit: Forest Plot', 'Position', [100 100 800 500]);
tiledlayout(1, 4, 'TileSpacing', 'compact');

for j = 1:4
    nexttile;
    data = delta_metrics(:,j);
    mu_val = mean(data);
    sem_val = std(data)/sqrt(length(data));
    ci95 = sem_val * 2.57; % Using ~2.57 for N=6 (t-distribution approx)
    
    % Plot individual participants as gray dots
    plot(data, 1:length(data), 'ko', 'MarkerFaceColor', [.8 .8 .8], 'MarkerEdgeColor', [.5 .5 .5]); 
    hold on;
    
    % Plot Group Mean and CI
    errorbar(mu_val, length(data)+1.5, ci95, 'horizontal', 'rs', ...
        'LineWidth', 2, 'MarkerSize', 10, 'MarkerFaceColor', 'r');
    
    % Formatting
    title(metric_labels{j}, 'FontSize', 10);
    grid on;
    set(gca, 'YTick', 1:length(data)+2, 'YTickLabel', [participants, {' ', 'GROUP'}]);
    line([0 0], [0 length(data)+2], 'Color', 'k', 'LineStyle', '--'); % Zero line
    
    if j == 1; ylabel('Participants'); end
    axis tight; 
end

sgtitle('tVNS-Induced Shifts in Decision Speed-Accuracy Tradeoff Curve (N=10)', 'FontSize', 14);