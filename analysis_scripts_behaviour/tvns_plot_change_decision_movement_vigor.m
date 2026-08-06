%% AGGREGATION SCRIPT: Calculate Deltas Per Subject (UPDATED with model fit)

% --- 1. Define Participants and File Structure ---
% List of all Participant IDs (SIDs)
participants = {'ALAN', 'LEA', 'FRTP01', 'FRTP02', 'FRTP03', 'FRTP04', 'FRTP06'}; % <-- REPLACE with your actual list of SIDs!
%% AGGREGATION SCRIPT: Calculate Deltas Per Subject (Reversed Delta: Sham - tVNS)

% --- 1. Define Participants and File Structure ---
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\tvns_experiment'; 
kinarm_delay = 0.02;

% Initialize table to store the final delta results
% Delta_Vel (Y-axis): Mean(peak_tanVel_Sham) - Mean(peak_tanVel_tVNS)
% Delta_Mu (X-axis):  mu_sham - mu_tVNS = -p5
subject_deltas = table('Size', [length(participants), 3], ...
                       'VariableTypes', {'string', 'double', 'double'}, ...
                       'VariableNames', {'SID', 'Delta_Vel', 'Delta_Mu'}); 

% --- 2. Loop Through Participants and Calculate Deltas ---
for p = 1:length(participants)
    pat = participants{p};
    
    % Construct file path and load data
    pat_dir = fullfile(base_dir, pat);
    tvns_file = fullfile(pat_dir, [pat, '_tvns_reach_data_analyzed.mat']);
    
    if exist(tvns_file, 'file') ~= 2
        warning('File not found for participant %s. Skipping.', pat);
        continue;
    end
    
    % Load the data (loads 'tvns_data_light' into workspace)
    load(tvns_file);
    tvns_data = tvns_data_light;
    
    % --- 2a. Calculate Mean Peak Velocity Delta (Sham - tVNS) ---
    STIM_SHAM = 1;
    STIM_TVNS = 2;
    
    sham_data = tvns_data(tvns_data.stim_type == STIM_SHAM, :);
    tvns_sub_data = tvns_data(tvns_data.stim_type == STIM_TVNS, :);
    
    % Use 'omitnan' to handle NaNs
    mean_vel_sham = mean(sham_data.peak_tanVel, 'omitnan');
    mean_vel_tvns = mean(tvns_sub_data.peak_tanVel, 'omitnan');
    
    % *** REVERSED CALCULATION ***
    delta_vel = mean_vel_tvns-mean_vel_sham; 

    % --- 2b. Prepare Inputs for Model Fit ---
    RT_combined = (tvns_data.eff_pt(:) / 1000)-kinarm_delay; 
    Hit_combined = tvns_data.success(:); 
    Stim_Condition_Input = tvns_data.stim_type(:); 
    
    % --- 2c. Execute Unified Model Fit ---
    Delta_mu = NaN; 
    try
        model_output = tvns_fit_unified_model(RT_combined, Hit_combined, Stim_Condition_Input);
        
        % The model's p5 is (tVNS - Sham). We want (Sham - tVNS), so we negate it.
        Delta_mu = model_output.paramsOpt(6); 
        
    catch ME
        warning('Model fit failed for %s. Error: %s. Setting model delta to NaN.', pat, ME.message);
    end
    
    % --- 2d. Store Results in the Aggregation Table ---
    subject_deltas.SID(p) = pat;
    subject_deltas.Delta_Vel(p) = delta_vel;
    subject_deltas.Delta_Mu(p) = Delta_mu; 
end

% Remove any rows where either delta calculation resulted in NaN 
nan_mask = isnan(subject_deltas.Delta_Mu) | isnan(subject_deltas.Delta_Vel);
subject_deltas(nan_mask, :) = [];

disp('--- Final Delta Results Per Subject (Sham - tVNS) ---');
disp(subject_deltas);

%% PLOTTING SCRIPT: Correlation Analysis (Updated Labels)

% 1. Prepare data (use the final filtered subject_deltas table)
X = subject_deltas.Delta_Mu;
Y = subject_deltas.Delta_Vel;

% Check for sufficient data points
if length(X) < 2
    warning('Insufficient data points for correlation plot (N < 2). Cannot calculate regression or R-value.');
    return;
end

% 2. Create the figure
figure;
hold on;
box on;
grid on;

% 3. Scatter Plot the individual subject points
scatter(X, Y, ...
        80, 'filled', ... 
        'MarkerFaceColor', [0.2 0.4 0.8], ...
        'MarkerEdgeColor', 'k');

% 4. Calculate and Plot the Regression Line
P = polyfit(X, Y, 1); 
X_fit = linspace(min(X), max(X), 100);
Y_fit = polyval(P, X_fit);

% Plot the regression line
plot(X_fit, Y_fit, 'r-', 'LineWidth', 2);

% 5. Calculate Correlation Coefficient (Pearson's r) and p-value
[R, Pval] = corr(X, Y, 'Type', 'Pearson');

% 6. Add axes lines at zero
%plot([0 0], ylim, 'k--', 'HandleVisibility', 'off'); 
%plot(xlim, [0 0], 'k--', 'HandleVisibility', 'off');

% 7. Add axes labels and title
xlabel('\Delta Mean Time (\Delta\mu) from Unified Model (Sham - tVNS, s)');
ylabel('\Delta Mean Peak Tangential Velocity (Sham - tVNS, units/s)');
title(sprintf('Correlation between \\Delta Mean Time and \\Delta Peak Velocity\n(r = %.2f, p = %.3f)', R, Pval));

hold off;