%% AGGREGATION SCRIPT: tVNS Vigor vs. Decision Speed (Flipped Sign)
participants = {'FRTP01', 'FRTP02', 'FRTP03', 'FRTP04', 'FRTP05', 'FRTP06', 'FRTP07', 'FRTP09', 'FRTP11', 'FRTP12', 'FRTP14', 'FRTP16', 'FRTP18', 'FRTP20', 'FRTP22', 'FRTP23'}; 

base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\tvns_experiment'; 
kinarm_delay = 0.02;

subject_deltas_tvns = table('Size', [length(participants), 3], ...
                       'VariableTypes', {'string', 'double', 'double'}, ...
                       'VariableNames', {'SID', 'Delta_Vel', 'Delta_Speed'}); 

fprintf('Processing tVNS Correlation Data...\n');

for p = 1:length(participants)
    pat = participants{p};
    disp(pat);
    pat_file = fullfile(base_dir, pat, [pat, '_tvns_reach_data_analyzed.mat']);
    if ~exist(pat_file, 'file'); continue; end
    L = load(pat_file);
    data = L.tvns_data_light; 
    
    SHAM = 1; ACTIVE = 2;
   
    % Load free response data
    pat_file_free = fullfile(base_dir, pat, [pat, '_free_response_reach_data_compact.mat']);
    load(pat_file_free);
    

    filter_outliers = @(data) data(data >= (quantile(data, 0.25) - 1.5*iqr(data)) & ...
                                   data <= (quantile(data, 0.75) + 1.5*iqr(data)));

    peak_tanVel_clean = filter_outliers(Compact.peak_tanVel(~isnan(Compact.peak_tanVel)));

    mean_free_peakVel = mean(peak_tanVel_clean);

    % --- 1. Calculate Velocity Delta ---
    tvns = 2;
    sham = 1;
   
    
    
    vel_sham_raw  = data.peak_tanVel(data.stim_type == sham);
    vel_tvns_raw = data.peak_tanVel(data.stim_type == tvns);
    
    vel_sham_clean  = filter_outliers(vel_sham_raw(~isnan(vel_sham_raw)));
    vel_tvns_clean = filter_outliers(vel_tvns_raw(~isnan(vel_tvns_raw)));
    
    vel_sham_pct = vel_sham_clean./mean_free_peakVel;
    vel_tvns_pct = vel_tvns_clean./mean_free_peakVel;

    mean_vel_sham = mean(vel_sham_pct)*100;
    mean_vel_tvns = mean(vel_tvns_pct)*100;

    

    delta_vel = mean_vel_tvns - mean_vel_sham; 
    disp("Sham: ");
    disp(mean_vel_sham);
    disp("tVNS: ")
    disp(mean_vel_tvns);
    
    % --- 2. Decision Speed Delta (Flipped Mu) ---
    VT = (data.eff_pt / 1000) - kinarm_delay;
    hit = data.success;
    idx_sham = (data.stim_type == SHAM & data.catch_trial == 1);
    idx_active = (data.stim_type == ACTIVE & data.catch_trial == 1);
    
    try
        m_sham = fit_model(VT(idx_sham), hit(idx_sham));
        m_active = fit_model(VT(idx_active), hit(idx_active));
        
        % SPEED TRANSFORMATION:
        % Delta_Time = mu_active - mu_sham (negative means faster)
        % Delta_Speed = -(mu_active - mu_sham) (positive means faster)
        delta_speed = -(m_active.paramsOpt(1) - m_sham.paramsOpt(1)); 
        
    catch ME
        delta_speed = NaN;
    end
    
    subject_deltas_tvns.SID(p) = pat;
    subject_deltas_tvns.Delta_Vel(p) = delta_vel;
    subject_deltas_tvns.Delta_Speed(p) = delta_speed; 
end

subject_deltas_tvns(isnan(subject_deltas_tvns.Delta_Speed), :) = [];

%% --- 3. Plotting: Decision Speed vs. Movement Speed ---
X = subject_deltas_tvns.Delta_Speed * 1000; % Convert to ms
Y = subject_deltas_tvns.Delta_Vel;

figure('Color', 'w', 'Name', 'tVNS Vigor Correlation');
hold on; grid on; box on;

% Scatter Plot
scatter(X, Y, 120, 'filled', 'MarkerFaceColor', [0.1 0.6 0.1], 'MarkerEdgeColor', 'k');
text(X + (0.02*range(X)), Y, subject_deltas_tvns.SID, 'FontSize', 9, 'FontWeight', 'bold');

% Regression and Stats
if length(X) > 2
    P = polyfit(X, Y, 1);
    plot([min(X)-5, max(X)+5], polyval(P, [min(X)-5, max(X)+5]), 'k-', 'LineWidth', 1.5);
    [R, Pval] = corr(X, Y, 'Type', 'Pearson');
end

xline(0, '--', 'Color', [0.5 0.5 0.5], 'HandleVisibility', 'off');
yline(0, '--', 'Color', [0.5 0.5 0.5], 'HandleVisibility', 'off');

% UPDATED LABELS
xlabel('\Delta Decision Speed (ms) [Right = Faster Onset under tVNS]');
ylabel('\Delta Movement Vigor (\Delta Peak Velocity)');
title({['tVNS Correlation: Decision Speed vs. Movement Vigor'], ...
       sprintf('(r = %.2f, p = %.3f, N = %d)', R, Pval, length(X))});

if R > 0 && Pval < 0.05
    fprintf('Significant Positive Correlation: Subjects who increased decision speed also increased movement vigor.\n');
end