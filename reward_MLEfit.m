%% Define Participant details
pat = 'FRTP33';
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\Reward_experiment'; 
pat_dir = strcat(base_dir, '\', pat);

reward_file = strcat(pat_dir, '\', pat, '_reward_reach_data_analyzed.mat');
load(reward_file);
exclude_blocks = [];
%% Define RT_array and Hit arrays (reward and non-reward)
reward_data = reward_data(reward_data.catch_trial == 1,:); % remove catch trials

no_reward_rt = reward_data.eff_pt(reward_data.reward_type == 3 & ~ismember(reward_data.block_num, exclude_blocks))/1000; % define rt array for no reward trials, divide by 1000 as fit_model expects data in seconds
reward_rt = reward_data.eff_pt(reward_data.reward_type == 4 & ~ismember(reward_data.block_num, exclude_blocks))/1000; % define rt array for reward trials

no_reward_hit = reward_data.success(reward_data.reward_type == 3 & ~ismember(reward_data.block_num, exclude_blocks)); % define hit array for no reward trials (1 = hit, 0 = miss)
reward_hit = reward_data.success(reward_data.reward_type == 4 & ~ismember(reward_data.block_num, exclude_blocks)); % define hit array for no reward trials


%% fit model to reward and no reward data
no_reward_model = fit_model(no_reward_rt, no_reward_hit);
reward_model = fit_model(reward_rt, reward_hit);

%% Visualize results

% Set up the axes and plot the first line (No Reward)
subplot(1,2,1);
plot(no_reward_model.p_hit_plot, 'b-'); % 'b-' means Blue, solid line
hold on; % Keep this plot on the axes

% Plot the second line (Reward) on the same axes
plot(reward_model.p_hit_plot, 'r--'); % 'r--' means Red, dashed line

ylim([0 1]);
xlim([0 500]);
title(strcat('Reward Exp Decision SAT - Subject:', pat));

% Add the legend. The labels are in the same order as the plot calls.
legend({'No Reward', 'Reward'}, 'Location', 'best');

hold off; % Release the plot for the next figure

subplot(1,2,2);

% 1. Plot the "No Reward" data point at x=1
% 'bo' specifies a blue circle marker, and 'MarkerFaceColor', 'b' fills it
plot(1, no_reward_model.paramsOpt(1), 'bo', 'MarkerSize', 8, 'MarkerFaceColor', 'b'); 
hold on; % Hold the axes to add the second point

% 2. Plot the "Reward" data point at x=2
% 'rs' specifies a red square marker, and 'MarkerFaceColor', 'r' fills it
plot(2, reward_model.paramsOpt(1), 'rs', 'MarkerSize', 8, 'MarkerFaceColor', 'r');

% Set limits and title
ylim([0.100 0.300]); 
xlim([0.5 2.5]); % Adjust X limits to center the points at 1 and 2
title(strcat('Mean of Prep Distribution - Subject:', pat));

% 3. Set the x-axis labels to correspond to the point positions
set(gca, 'XTick', [1 2], 'XTickLabel', {'No Reward', 'Reward'});
xlabel(''); % Remove the default x-label if any



hold off; % Release the plot