%% AGGREGATION SCRIPT: tVNS Vigor vs. Decision Speed (Flipped Sign)
participants = {'FRTP01', 'FRTP02', 'FRTP03', 'FRTP04', 'FRTP05', 'FRTP06', ...
                'FRTP07', 'FRTP09', 'FRTP11', 'FRTP12', 'FRTP14', 'FRTP16', ...
                'FRTP17', 'FRTP18', 'FRTP20', 'FRTP21', 'FRTP22', 'FRTP23', ...
                'FRTP24', 'FRTP25', 'FRTP26', 'FRTP27', 'FRTP28', 'FRTP29', ...
                'FRTP30', 'FRTP31'};
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
   
    % --- 1. Velocity Delta ---
    filter_outliers = @(d) d(d >= (quantile(d, 0.25) - 1.5*iqr(d)) & ...
                             d <= (quantile(d, 0.75) + 1.5*iqr(d)));
    
    v_sham = filter_outliers(data.peak_tanVel(data.stim_type == SHAM & ~isnan(data.peak_tanVel)));
    v_act  = filter_outliers(data.peak_tanVel(data.stim_type == ACTIVE & ~isnan(data.peak_tanVel)));
    delta_vel = mean(v_act) - mean(v_sham); 
    
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
        delta_mu = (m_active.paramsOpt(1) - m_sham.paramsOpt(1)); 
        delta_sigma = (m_active.paramsOpt(2) - m_sham.paramsOpt(2)); 
        
    catch ME
        delta_speed = NaN;
    end
    
    subject_deltas_tvns.SID(p) = pat;
    subject_deltas_tvns.Delta_Vel(p) = delta_vel;
    subject_deltas_tvns.Delta_mu(p) = delta_mu; 
    subject_deltas_tvns.Delta_sigma(p) = delta_sigma; 
end

subject_deltas_tvns(isnan(subject_deltas_tvns.Delta_mu), :) = [];

%% --- 3. Plotting: Decision Speed vs. Movement Speed ---
X = subject_deltas_tvns.Delta_mu * 1000; % Convert to ms
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