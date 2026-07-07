%% Define Participant details
pat = 'FRTP14';
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\tvns_experiment'; 
pat_dir = strcat(base_dir, '\', pat);

tvns_file = strcat(pat_dir, '\', pat, '_tvns_reach_data_analyzed.mat');
load(tvns_file);
tvns_data = tvns_data_light;

sham_exclude_blocks = [];
tvns_exclude_blocks = [];
kinarm_delay = 0.02;
%% Define RT_array and Hit arrays (tvns and non-tvns)
tvns_data = tvns_data(tvns_data.catch_trial == 1,:); % remove catch trials

sham_rt = tvns_data.eff_pt(tvns_data.stim_type == 1 & ~ismember(tvns_data.block_num, sham_exclude_blocks))/1000; % define rt array for no tvns trials, divide by 1000 as fit_model expects data in seconds
tvns_rt = tvns_data.eff_pt(tvns_data.stim_type == 2 & ~ismember(tvns_data.block_num, tvns_exclude_blocks))/1000; % define rt array for tvns trials

sham_hit = tvns_data.success(tvns_data.stim_type == 1 & ~ismember(tvns_data.block_num, sham_exclude_blocks)); % define hit array for no tvns trials (1 = hit, 0 = miss)
tvns_hit = tvns_data.success(tvns_data.stim_type == 2 & ~ismember(tvns_data.block_num, tvns_exclude_blocks)); % define hit array for no tvns trials

%% fit model to tvns and no tvns data
sham_model = fit_model(sham_rt-kinarm_delay, sham_hit);
tvns_model = fit_model(tvns_rt-kinarm_delay, tvns_hit);

%% Visualize results
figure('Color', 'w', 'Name', 'tVNS MLE Fit');

% Set up the axes and plot the first line (sham)
subplot(1,2,1);
plot(sham_model.p_hit_plot, 'b-'); % 'b-' means Blue, solid line
hold on; % Keep this plot on the axes

% Plot the second line (tvns) on the same axes
plot(tvns_model.p_hit_plot, 'r--'); % 'r--' means Red, dashed line

ylim([0 1]);
xlim([0 500]);
title(strcat('tVNS Exp Decision Speed Accuracy Curve'));
xlabel('Preparation Time (s)');
ylabel('Probability of Target Hit');

% Add the legend. The labels are in the same order as the plot calls.
legend({'sham', 'tvns'}, 'Location', 'best');

hold off; % Release the plot for the next figure

subplot(1,2,2);

% 1. Plot the "No tvns" data point at x=1
% 'bo' specifies a blue circle marker, and 'MarkerFaceColor', 'b' fills it
plot(1, sham_model.paramsOpt(1), 'bo', 'MarkerSize', 8, 'MarkerFaceColor', 'b'); 
hold on; % Hold the axes to add the second point

% 2. Plot the "tvns" data point at x=2
% 'rs' specifies a red square marker, and 'MarkerFaceColor', 'r' fills it
plot(2, tvns_model.paramsOpt(1), 'rs', 'MarkerSize', 8, 'MarkerFaceColor', 'r');

% Set limits and title
ylim([0.05 0.300]); 
xlim([0.5 2.5]); % Adjust X limits to center the points at 1 and 2
title(strcat('Mean of Preparation Distribution'));
ylabel('Average Preparation Time (s)');
xlabel('Stimulation Condition');

% 3. Set the x-axis labels to correspond to the point positions
set(gca, 'XTick', [1 2], 'XTickLabel', {'Sham', 'tVNS'});



hold off; % Release the plot
disp("Sham sigma: ")
disp(sham_model.paramsOpt(2));
disp("tVNS sigma: ")
disp(tvns_model.paramsOpt(2));
disp("Change: ")
disp(tvns_model.paramsOpt(2) - sham_model.paramsOpt(2))