%% Define Participant details
pat = 'FRTC04'; % Update for your new participant
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\Combined_experiment'; % Update path as needed
pat_dir = strcat(base_dir, '\', pat);
data_file = strcat(pat_dir, '\', pat, '_combined_reach_data_analyzed.mat');
load(data_file);

exclude_blocks = [];

%% Define RT and Hit arrays for all 4 conditions
% I've mapped this to 'data' to make it generic. Adjust table name if needed.
data = combined_data; 
data = data(data.catch_trial == 1,:); % remove catch trials

% --- Condition 1: Sham + No Reward ---
idx_sham_norew = (data.stim_type == 1 & data.reward_type == 3 & ~ismember(data.block_num, exclude_blocks));
sham_norew_rt  = data.eff_pt(idx_sham_norew)/1000;
sham_norew_hit = data.success(idx_sham_norew);

% --- Condition 2: Sham + Reward ---
idx_sham_rew = (data.stim_type == 1 & data.reward_type == 4 & ~ismember(data.block_num, exclude_blocks));
sham_rew_rt  = data.eff_pt(idx_sham_rew)/1000;
sham_rew_hit = data.success(idx_sham_rew);

% --- Condition 3: tVNS + No Reward ---
idx_tvns_norew = (data.stim_type == 2 & data.reward_type == 3 & ~ismember(data.block_num, exclude_blocks));
tvns_norew_rt  = data.eff_pt(idx_tvns_norew)/1000;
tvns_norew_hit = data.success(idx_tvns_norew);

% --- Condition 4: tVNS + Reward ---
idx_tvns_rew = (data.stim_type == 2 & data.reward_type == 4 & ~ismember(data.block_num, exclude_blocks));
tvns_rew_rt  = data.eff_pt(idx_tvns_rew)/1000;
tvns_rew_hit = data.success(idx_tvns_rew);

%% Fit model to the 4 datasets independently
sham_norew_model = fit_model(sham_norew_rt, sham_norew_hit);
sham_rew_model   = fit_model(sham_rew_rt, sham_rew_hit);
tvns_norew_model = fit_model(tvns_norew_rt, tvns_norew_hit);
tvns_rew_model   = fit_model(tvns_rew_rt, tvns_rew_hit);

%% Visualize results
% Set consistent colors to match your R plots
c_sham_norew = [0.1 0.1 0.1];  % Gray/Black (Sham + No Reward)
c_sham_rew   = [0.2 0.6 0.86]; % Blue (Sham + Reward)
c_tvns_norew = [0 0.7 0];      % Green (tVNS + No Reward)
c_tvns_rew   = [0.9 0.3 0.24]; % Red (tVNS + Reward)

% Create a wider figure for the two subplots
figure('Name', strcat('2x2 Individual Fits - Subject: ', pat), 'Position', [100 100 1000 400]);

% ---------------------------------------------------------
% Subplot 1: Psychometric Curves
% ---------------------------------------------------------
subplot(1,2,1);
hold on; 

% Plot all 4 fitted curves
plot(sham_norew_model.p_hit_plot, 'Color', c_sham_norew, 'LineWidth', 2, 'LineStyle', '-');
plot(sham_rew_model.p_hit_plot,   'Color', c_sham_rew,   'LineWidth', 2, 'LineStyle', '-');
plot(tvns_norew_model.p_hit_plot, 'Color', c_tvns_norew, 'LineWidth', 2, 'LineStyle', '-');
plot(tvns_rew_model.p_hit_plot,   'Color', c_tvns_rew,   'LineWidth', 2, 'LineStyle', '-');

ylim([0 1]);
xlim([0 400]); % Kept matching your original script layout
title(strcat('Decision SAT - Subject:', pat));
xlabel('Time');
ylabel('P(Hit)');
legend({'Sham + No Reward', 'Sham + Reward', 'tVNS + No Reward', 'tVNS + Reward'}, 'Location', 'best');
hold off; 

% ---------------------------------------------------------
% Subplot 2: Mu Parameters (\mu)
% ---------------------------------------------------------
subplot(1,2,2);
hold on;

% Plot the 4 data points at x = 1, 2, 3, 4 with distinct markers
plot(1, sham_norew_model.paramsOpt(1), 'o', 'MarkerSize', 9, 'MarkerFaceColor', c_sham_norew, 'MarkerEdgeColor', 'k'); 
plot(2, sham_rew_model.paramsOpt(1),   's', 'MarkerSize', 9, 'MarkerFaceColor', c_sham_rew,   'MarkerEdgeColor', 'k');
plot(3, tvns_norew_model.paramsOpt(1), '^', 'MarkerSize', 9, 'MarkerFaceColor', c_tvns_norew, 'MarkerEdgeColor', 'k');
plot(4, tvns_rew_model.paramsOpt(1),   'd', 'MarkerSize', 9, 'MarkerFaceColor', c_tvns_rew,   'MarkerEdgeColor', 'k');

% Set limits and title
% Note: Widened the Y-axis slightly just in case your new data varies more
ylim([0.050 0.350]); 
xlim([0.5 4.5]); 
title(strcat('Mean of Prep Distribution (\mu) - Subject:', pat));
ylabel('Estimated \mu (s)');

% Set the x-axis labels to correspond to the point positions
set(gca, 'XTick', 1:4, 'XTickLabel', {'Sham+NoRew', 'Sham+Rew', 'tVNS+NoRew', 'tVNS+Rew'});
xtickangle(20); % Angle text slightly to prevent overlapping
hold off;