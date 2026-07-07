%% Define Group Details
 participants =  {'FRTP03', 'FRTP11', 'FRTP12', 'FRTP13', ...
                'FRTP14'}%{'FRTP01', 'FRTP02', 'FRTP03', 'FRTP04', 'FRTP05', 'FRTP06', ...
%                 'FRTP07', 'FRTP08', 'FRTP09', 'FRTP11', 'FRTP12', 'FRTP13', ...
%                 'FRTP14', 'ALAN', 'LEA', 'RONAN'};

base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\Reward_experiment'; 

all_rt = []; % Initialize empty vector to store group data
all_reward_condition = [];
all_success = [];
%% Loop through participants
for i = 1:length(participants)
    pat = participants{i};
    pat_dir = fullfile(base_dir, pat); % Using fullfile is safer for paths
    reward_file = fullfile(pat_dir, [pat, '_reward_reach_data_analyzed.mat']);
    
    % Check if file exists before loading to avoid crashes
    if exist(reward_file, 'file')
        load(reward_file);
        % Extract angles for catch_trial == 2
        % Ensure we are grabbing a column vector for consistent concatenation
        current_rt = reward_data.rt(reward_data.catch_trial == 1 & reward_data.prep_time < 100);
        reward_condition = reward_data.reward_type(reward_data.catch_trial == 1 & reward_data.prep_time < 100);
        success = reward_data.success(reward_data.catch_trial == 1 & reward_data.prep_time < 100);


        all_rt = [all_rt; current_rt(:)];
        all_reward_condition = [all_reward_condition; reward_condition(:)];
        all_success = [all_success; success(:)];
    else
        fprintf('Warning: File not found for participant %s\n', pat);
    end
end

%% Create the Figure
figure('Color', 'w', 'Position', [100, 100, 1000, 450]);
t = tiledlayout(1, 3, 'TileSpacing', 'Compact', 'Padding', 'Compact');

% Pre-allocate axes array to link them later
ax = gobjects(1, 2); 

% Define plot settings
colors = {[0.8, 0.2, 0.2], [0.2, 0.6, 0.2]}; % Red (Fail), Green (Success)
bin_edges = linspace(min(all_rt), max(all_rt), 25); % Unified bins

% Loop through Stim Conditions
titles = {'Condition 1: No Reward', 'Condition 2: Reward'};

for s = 1:2
    ax(s) = nexttile; 
    hold on;
    
    stim_mask = (all_reward_condition == s+2);
    
    % Plot distributions
    histogram(all_rt(stim_mask & all_success == 0), bin_edges, ...
        'FaceColor', colors{1}, 'FaceAlpha', 0.4, 'DisplayName', 'Failure (0)');
    
    histogram(all_rt(stim_mask & all_success == 1), bin_edges, ...
        'FaceColor', colors{2}, 'FaceAlpha', 0.4, 'DisplayName', 'Success (1)');
    
    title(titles{s});
    xlabel('Preparation Time');
    ylabel('Frequency');
    grid on;
    if s == 1, legend('Location', 'northeast'); end
    if s == 2
        ax(3) = nexttile; 
        hold on
        histogram(all_rt(all_success == 0), bin_edges, ...
        'FaceColor', colors{1}, 'FaceAlpha', 0.4, 'DisplayName', 'Failure (0)');
        histogram(all_rt(all_success == 1), bin_edges, ...
        'FaceColor', colors{2}, 'FaceAlpha', 0.4, 'DisplayName', 'Success (1)');
        title(titles{s});
        xlabel('Preparation Time');
        ylabel('Frequency');
        grid on;
    end
end

%% The "Common Axes" Logic
% 1. Link the axes so zooming/panning happens on both simultaneously
linkaxes(ax, 'xy'); 

% 2. Manually set limits based on the global max/min of the data
% This ensures the white space is consistent across both tiles
all_y_limits = cell2mat(get(ax, 'YLim')); 
set(ax, 'YLim', [0, max(all_y_limits(:,2))]); % Sync Y-axis to highest count
set(ax, 'XLim', [min(bin_edges), max(bin_edges)]); % Sync X-axis to data range

title(t, 'Reaction Time Distributions: Side-by-Side Comparison', ...
    'FontSize', 14, 'FontWeight', 'bold');