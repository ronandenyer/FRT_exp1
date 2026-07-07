%% Define Participant details
pat = 'FRTP06';
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\tvns_experiment'; 
pat_dir = strcat(base_dir, '\', pat);

tvns_file = strcat(pat_dir, '\', pat, '_tvns_reach_data_analyzed.mat');
load(tvns_file);
tvns_data = tvns_data_light;
kinarm_delay = 0.02;

sham_exclude_blocks = [];
tvns_exclude_blocks = [];

%% Define RT_array and Hit arrays (tvns and non-tvns)
if height(tvns_data) == 416
    tvns_data.prev_hit = [NaN; tvns_data.feedback_success(1:end-1)];
    tvns_data.prev_hit([53 105 157 209 261 313 365]) = NaN; % pre vit not valid for first trials on a block
elseif height(tvns_data) == 432
    tvns_data.prev_hit = [NaN; tvns_data.feedback_success(1:end-1)];
    tvns_data.prev_hit([55 109 163 217 271 325 379]) = NaN; % pre vit not valid for first trials on a block
end

tvns_data = tvns_data(tvns_data.catch_trial == 1,:); % remove catch trials


rt = (tvns_data.eff_pt(~ismember(tvns_data.block_num, sham_exclude_blocks))/1000) - kinarm_delay; % define rt array for no tvns trials, divide by 1000 as fit_model expects data in seconds

hit = tvns_data.success(~ismember(tvns_data.block_num, sham_exclude_blocks)); % define hit array for no tvns trials (1 = hit, 0 = miss)

stim_condition = tvns_data.stim_type(~ismember(tvns_data.block_num, sham_exclude_blocks));

%%
% Assuming your data table is called 'tvns_data'
% 1. Create a logical index for the trial before
% We shift the hit vector by 1 to align 'previous trial outcome' with 'current trial'
prev_hit = tvns_data.prev_hit;

% 2. Identify indices for the two models
idx_after_correct = (prev_hit == 1);
idx_after_error   = (prev_hit == 0);

% 3. Extract data for Model A (Post-Correct)
RT_correct = tvns_data.eff_pt(idx_after_correct) / 1000;
Hit_correct = tvns_data.success(idx_after_correct);
Cond_correct = tvns_data.stim_type(idx_after_correct);

% 4. Extract data for Model B (Post-Error)
RT_error = tvns_data.eff_pt(idx_after_error) / 1000;
Hit_error = tvns_data.success(idx_after_error);
Cond_error = tvns_data.stim_type(idx_after_error);

% Run for Post-Correct Trials
model_post_correct = fit_sequential_unified_model(RT_correct, Hit_correct, Cond_correct);

% Run for Post-Error Trials
model_post_error = fit_sequential_unified_model(RT_error, Hit_error, Cond_error);

% Compare the Delta_Mu (p5) across the two states
delta_mu_after_correct = model_post_correct.paramsOpt(5);
delta_mu_after_error   = model_post_error.paramsOpt(5);

delta_sigma_after_correct = model_post_correct.paramsOpt(7);
delta_sigma_after_error   = model_post_error.paramsOpt(7);

delta_beta_after_correct = model_post_correct.paramsOpt(6);
delta_beta_after_error   = model_post_error.paramsOpt(6);

%% PLOTTING: Post-Correct vs Post-Error SAT Curves
% This script assumes you have already run your fitting function twice:
% model_post_correct = fit_sequential_unified_model(RT_correct, Hit_correct, Cond_correct);
% model_post_error   = fit_sequential_unified_model(RT_error, Hit_error, Cond_error);

figure('Position', [100, 100, 1100, 500], 'Color', 'w'); 

% Define Colors
color_sham = [0.4 0.4 0.4]; % Dark Gray
color_tvns = [0.0 0.45 0.74]; % Blue

% Check for "Z-shape" (hitting the 0.005 lower bound)
if model_post_error.paramsOpt(2) <= 0.006
    fprintf('Warning: model_post_error sigma is at boundary. Curve may look vertical.\n');
end

% --- Subplot 1: After Correct Trials ---
subplot(1, 2, 1);
hold on; grid on; box on;

% Plot Curves
p1 = plot(model_post_correct.t_plot, model_post_correct.p_hit_plot_sham, ...
    'Color', color_sham, 'LineWidth', 3);
p2 = plot(model_post_correct.t_plot, model_post_correct.p_hit_plot_stim, ...
    'Color', color_tvns, 'LineWidth', 3);

title('Post-Correct Performance', 'FontSize', 13);
xlabel('Time (s)', 'FontSize', 11);
ylabel('P(Hit)', 'FontSize', 11);
ylim([0 1]); xlim([0 0.5]);
legend([p1, p2], {'Sham', 'tVNS'}, 'Location', 'southeast', 'FontSize', 10);

% --- Subplot 2: After Error Trials ---
subplot(1, 2, 2);
hold on; grid on; box on;



% Plot Curves (using dashed lines to distinguish from Post-Correct)
p3 = plot(model_post_error.t_plot, model_post_error.p_hit_plot_sham, ...
    'Color', color_sham, 'LineWidth', 3, 'LineStyle', '--');
p4 = plot(model_post_error.t_plot, model_post_error.p_hit_plot_stim, ...
    'Color', color_tvns, 'LineWidth', 3, 'LineStyle', '--');

% Calculate PES (Post-Error Slowing) for the Title
pes_sham = (model_post_error.paramsOpt(1) - model_post_correct.paramsOpt(1)) * 1000; % ms

title(['Post-Error Performance (PES \approx ', num2str(round(pes_sham)), ' ms)'], 'FontSize', 13);
xlabel('Time (s)', 'FontSize', 11);
ylabel('P(Hit)', 'FontSize', 11);
ylim([0 1]); xlim([0 0.5]);
legend([p3, p4], {'Sham (Post-Err)', 'tVNS (Post-Err)'}, 'Location', 'southeast', 'FontSize', 10);

% Overall Figure Title
sgtitle('Sequential Dynamics: The Impact of Previous Trial Outcome on SAT', 'FontSize', 15, 'FontWeight', 'bold');