%% Define Participant details
pat = 'LEA';
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\tvns_experiment'; 
pat_dir = strcat(base_dir, '\', pat);

tvns_file = strcat(pat_dir, '\', pat, '_tvns_reach_data_analyzed.mat');
load(tvns_file);
tvns_data = tvns_data_light;

sham_exclude_blocks = [];
tvns_exclude_blocks = [];
kinarm_delay = 0.02;
%% 1. Extract and Clean Data
tvns_data_clean = tvns_data(tvns_data.catch_trial == 1,:); 

sham_raw = tvns_data_clean.peak_tanVel(tvns_data_clean.stim_type == 1 & ~ismember(tvns_data_clean.block_num, sham_exclude_blocks));
tvns_raw = tvns_data_clean.peak_tanVel(tvns_data_clean.stim_type == 2 & ~ismember(tvns_data_clean.block_num, tvns_exclude_blocks));

% Remove Outliers
[sham_peakVel, ~] = rmoutliers(sham_raw, 'median');
[tvns_peakVel, ~] = rmoutliers(tvns_raw, 'median');

% Calculate Means
m_sham = mean(sham_peakVel);
m_tvns = mean(tvns_peakVel);

%% 2. Plotting with Fixed Color Logic
figure('Color', 'w');
hold on;

% Define colors explicitly to prevent confusion
color_sham = [0 0.447 0.741];  % Blue
color_tvns = [0.85 0.325 0.098]; % Orange

% Histograms
h1 = histogram(sham_peakVel, 'FaceAlpha', 0.5, 'FaceColor', color_sham, 'EdgeColor', 'none', 'DisplayName', 'Sham');
h2 = histogram(tvns_peakVel, 'FaceAlpha', 0.5, 'FaceColor', color_tvns, 'EdgeColor', 'none', 'DisplayName', 'tVNS');

% Adjust Bin Width (Halved)
newWidth = h1.BinWidth / 2;
h1.BinWidth = newWidth;
h2.BinWidth = newWidth;

% Add Vertical Mean Lines - Using the explicit color variables
yl = ylim;
line([m_sham m_sham], [0 yl(2)], 'Color', color_sham, 'LineWidth', 2, 'LineStyle', '--', 'HandleVisibility', 'off');
line([m_tvns m_tvns], [0 yl(2)], 'Color', color_tvns, 'LineWidth', 2, 'LineStyle', '--', 'HandleVisibility', 'off');

% Dynamic Text Labels (Positioned above the mean lines)
% This ensures the label color ALWAYS matches the line color
text(m_sham, yl(2)*0.95, sprintf(' Sham Mean: %.3f', m_sham), 'Color', color_sham, 'FontWeight', 'bold', 'VerticalAlignment', 'top');
text(m_tvns, yl(2)*0.85, sprintf(' tVNS Mean: %.3f', m_tvns), 'Color', color_tvns, 'FontWeight', 'bold', 'VerticalAlignment', 'top');

% Final Formatting
xlabel('Peak Reach Velocity (m/s)');
ylabel('Frequency (Trial Count)');
title(['Peak Reach Velocity (Outliers Removed) - ', pat]);
legend('Location', 'northeast');
grid on;
hold off;