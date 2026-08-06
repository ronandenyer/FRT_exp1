%% Define Study Details
participants = {'FRTP01', 'FRTP02', 'FRTP03', 'FRTP04', 'FRTP05', 'FRTP06', ...
                'FRTP07', 'FRTP09', 'FRTP11', 'FRTP12', 'FRTP14', 'FRTP16', ...
                'FRTP17', 'FRTP18', 'FRTP20', 'FRTP21', 'FRTP22', 'FRTP23', ...
                'FRTP24', 'FRTP25', 'FRTP26', 'FRTP27', 'FRTP28', 'FRTP29', ...
                'FRTP30', 'FRTP31', 'FRTP32', 'FRTP33'};
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\tvns_experiment'; 

% Parameters
WINDOW_SIZE_POINTS = 100; 
STIM_SHAM = 1;
STIM_TVNS = 2;
COLOR_SHAM = [0 0.4470 0.7410]; 
COLOR_TVNS = [0.8500 0.3250 0.0980];

% Create a common time axis for interpolation (0 to 100% of task or max eff_pt)
% Adjust '1000' to the desired resolution of your final plot
common_time = linspace(0, 500, 1000)'; 

% Storage for interpolated data: [time_point x participant]
sham_matrix = NaN(length(common_time), length(participants));
tvns_matrix = NaN(length(common_time), length(participants));

%% 1. Loop through Participants and Process Data
for p = 1:length(participants)
    pat = participants{p};
    fprintf('Processing participant: %s...\n', pat);
    
    try
        pat_dir = fullfile(base_dir, pat);
        tvns_file = fullfile(pat_dir, sprintf('%s_tvns_reach_data_analyzed.mat', pat));
        load(tvns_file, 'tvns_data_light');
        tvns_data = tvns_data_light;

        pat_file_free = fullfile(base_dir, pat, [pat, '_free_response_reach_data_compact.mat']);
        load(pat_file_free);
        
    
        filter_outliers = @(data) data(data >= (quantile(data, 0.25) - 1.5*iqr(data)) & ...
                                       data <= (quantile(data, 0.75) + 1.5*iqr(data)));
    
        peak_tanVel_clean = filter_outliers(Compact.peak_tanVel(~isnan(Compact.peak_tanVel)));
    
        mean_free_peakVel = mean(peak_tanVel_clean);
        
        for condition_idx = 1:2
            curr_type = iff(condition_idx == 1, STIM_SHAM, STIM_TVNS);
            
            % Filter and Sort
            data_sub = tvns_data(tvns_data.stim_type == curr_type, :);
            data_sub(isnan(data_sub.peak_tanVel) | isnan(data_sub.eff_pt), :) = [];
            data_sub = sortrows(data_sub, 'eff_pt');
            

            if ~isempty(data_sub)
                % Apply smoothing at individual level
                norm_peak_tanVel = data_sub.peak_tanVel/mean_free_peakVel;
                smoothed_vel = movmean(norm_peak_tanVel, WINDOW_SIZE_POINTS);
                
                % Interpolate onto common time axis so we can average across subjects
                % 'unique' handles cases where multiple trials might have same eff_pt
                [unique_x, idx] = unique(data_sub.eff_pt);
                interp_vel = interp1(unique_x, smoothed_vel(idx), common_time, 'linear', NaN);
                
                if condition_idx == 1
                    sham_matrix(:, p) = interp_vel;
                else
                    tvns_matrix(:, p) = interp_vel;
                end
            end
        end
    catch ME
        fprintf('Could not process %s: %s\n', pat, ME.message);
    end
end

%% 2. Calculate Grand Averages and Between-Subject SEM
% We ignore NaNs (where participants didn't have data for that time point)
ga_sham = nanmean(sham_matrix, 2);
ga_tvns = nanmean(tvns_matrix, 2);

sem_sham = nanstd(sham_matrix, 0, 2) ./ sqrt(sum(~isnan(sham_matrix), 2));
sem_tvns = nanstd(tvns_matrix, 0, 2) ./ sqrt(sum(~isnan(tvns_matrix), 2));

%% 1. [Keep the Data Loading & Interpolation section from the previous code]
% ... (Ensure sham_matrix and tvns_matrix are populated) ...

%% 2. Calculate Grand Averages and Between-Subject SEM
ga_sham = nanmean(sham_matrix, 2);
ga_tvns = nanmean(tvns_matrix, 2);

sem_sham = nanstd(sham_matrix, 0, 2) ./ sqrt(sum(~isnan(sham_matrix), 2));
sem_tvns = nanstd(tvns_matrix, 0, 2) ./ sqrt(sum(~isnan(tvns_matrix), 2));

%% 3. Create the Plot with Individual Traces
figure('Color', 'w', 'Units', 'normalized', 'Position', [0.1 0.1 0.6 0.7]);
hold on; grid on; box on;

% Plot Individual Traces First (so they sit behind the Grand Average)
% We use a loop for each participant and set a faint Alpha value
for p = 1:length(participants)
    % Sham individual trace
    plot(common_time, sham_matrix(:,p), 'Color', [COLOR_SHAM, 0.15], ...
        'LineWidth', 0.5, 'HandleVisibility', 'off'); 
    
    % tVNS individual trace
    plot(common_time, tvns_matrix(:,p), 'Color', [COLOR_TVNS, 0.15], ...
        'LineWidth', 0.5, 'HandleVisibility', 'off');
end

% Plot Grand Average Shaded SEM
fill_sham_x = [common_time; flipud(common_time)];
fill_sham_y = [ga_sham + sem_sham; flipud(ga_sham - sem_sham)];
fill(fill_sham_x(~isnan(fill_sham_y)), fill_sham_y(~isnan(fill_sham_y)), ...
    COLOR_SHAM, 'FaceAlpha', 0.3, 'EdgeColor', 'none', 'HandleVisibility', 'off');

fill_tvns_x = [common_time; flipud(common_time)];
fill_tvns_y = [ga_tvns + sem_tvns; flipud(ga_tvns - sem_tvns)];
fill(fill_tvns_x(~isnan(fill_tvns_y)), fill_tvns_y(~isnan(fill_tvns_y)), ...
    COLOR_TVNS, 'FaceAlpha', 0.3, 'EdgeColor', 'none', 'HandleVisibility', 'off');

% Plot Bold Grand Average Lines
p1 = plot(common_time, ga_sham, 'Color', COLOR_SHAM, 'LineWidth', 3, 'DisplayName', 'Sham (Grand Avg)');
p2 = plot(common_time, ga_tvns, 'Color', COLOR_TVNS, 'LineWidth', 3, 'DisplayName', 'tVNS (Grand Avg)');

%% 4. Customization
xlabel('Effective Point (eff\_pt, ms)');
ylabel('Peak Tangential Velocity');
title(['Grand Average vs. Individual Traces (n=' num2str(length(participants)) ')']);
legend([p1, p2], 'Location', 'best');

% Set axes limits to fit data tightly
xlim([min(common_time) max(common_time)]);
hold off;
%% Helper Function (Add this at the very bottom of your script)
function res = iff(condition, true_val, false_val)
    if condition, res = true_val; else, res = false_val; end
end