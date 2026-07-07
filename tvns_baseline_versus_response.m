%% --- AGGREGATION & CORRELATION SCRIPT: Mu, Sigma, & Global Efficiency ---
participants = {'FRTP01', 'FRTP02', 'FRTP03', 'FRTP04', 'FRTP05', 'FRTP06', ...
                'FRTP07', 'FRTP09', 'FRTP11', 'FRTP12', ...
                'FRTP14', 'FRTP16', 'FRTP17', 'FRTP18', 'FRTP20', 'FRTP21',  ...
                'FRTP22','FRTP23', 'FRTP24', 'FRTP25', 'FRTP26', 'FRTP27', 'FRTP28', ...
                'FRTP29', 'FRTP30', 'FRTP31', 'FRTP32', 'FRTP33'};

base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\tVNS_experiment'; 
kinarm_delay = 0.0; 

% Preallocate validResults table (Added columns for Sigma and Global Efficiency)
validResults = table('Size', [length(participants) 10], ...
    'VariableTypes', {'string', 'double', 'double', 'double', 'double', 'double', 'double', 'double', 'double', 'double'}, ...
    'VariableNames', {'SID', 'Sham_mu', 'Active_mu', 'Change', ...
                              'Sham_sigma', 'Active_sigma', 'ChangeSigma', ...
                              'Sham_GlobEff', 'Active_GlobEff', 'ChangeGlobEff'});

fprintf('Processing Individual Fits per Participant...\n');

tmin = 0;   % choose meaningful range
tmax = 0.5;


for p = 1:length(participants)
    pat = participants{p};
    disp(['Processing: ' pat]);
    
    pat_file = fullfile(base_dir, pat, [pat, '_tvns_reach_data_analyzed.mat']);
    if ~exist(pat_file, 'file'); continue; end
    L = load(pat_file);
    data = L.tvns_data_light; 
    
    SHAM = 1; ACTIVE = 2;                
    valid_trials = (~isnan(data.eff_pt) & data.catch_trial == 1);                
    data = data(valid_trials, :);
    
    % Sham
    idx_sham = (data.stim_type == SHAM);
    sham_model = fit_model((data.eff_pt(idx_sham) / 1000) - kinarm_delay, data.success(idx_sham));
    validResults.Sham_mu(p) = sham_model.paramsOpt(1); 
    validResults.Sham_sigma(p) = sham_model.paramsOpt(2);

    validResults.Sham_GlobEff(p) = trapz(sham_model.t_plot, sham_model.p_hit_plot);  % mean predicted accuracy
    validResults.Sham_asym(p) = sham_model.paramsOpt(4);

    % Active
    idx_active = (data.stim_type == ACTIVE);
    active_model = fit_model((data.eff_pt(idx_active) / 1000) - kinarm_delay, data.success(idx_active));
    validResults.Active_mu(p) = active_model.paramsOpt(1);
    validResults.Active_sigma(p) = active_model.paramsOpt(2);

    validResults.Active_GlobEff(p) = trapz(active_model.t_plot, active_model.p_hit_plot); % mean predicted accuracy
    validResults.Active_asym(p) = active_model.paramsOpt(4);
    validResults.SID(p) = pat;

    % Define the integration range (e.g., from 0 to 500ms)
    t_range = sham_model.t_plot;
    
    % Calculate AUC for Sham
    AUC_sham = trapz(t_range, sham_model.p_hit_plot);
    
    % Calculate AUC for Stim
    AUC_stim = trapz(t_range, active_model.p_hit_plot);
    
    % The 'Global Response' is the Change in Area
    validResults.PEI_Change(p) = AUC_stim - AUC_sham;
    validResults.PEI_Baseline(p) = AUC_sham;
end

% --- 3. Calculate Change Scores ---
validResults.Change = validResults.Active_mu - validResults.Sham_mu;
validResults.ChangeSigma = validResults.Active_sigma - validResults.Sham_sigma;
validResults.ChangeAsym = validResults.Active_asym - validResults.Sham_asym;
validResults.ChangeGlobEff = validResults.Active_GlobEff - validResults.Sham_GlobEff;

% Remove subjects with failed fits (NaN)
validResults = validResults(~isnan(validResults.Sham_mu) & ~isnan(validResults.Active_mu), :);

%% --- 4. Visualize Mu Baseline Dependency (Original) ---
[r_mu, p_mu] = corr(validResults.Sham_mu, validResults.Change, 'Type', 'Spearman');
figure('Color', 'w', 'Name', 'Baseline Dependency (Mu)');
hold on; grid on;
scatter(validResults.Sham_mu, validResults.Change, 70, 'filled', 'MarkerEdgeColor', 'k');
text(validResults.Sham_mu + 0.005, validResults.Change, validResults.SID, 'FontSize', 8, 'Interpreter', 'none');
if height(validResults) > 2
    coeffs = polyfit(validResults.Sham_mu, validResults.Change, 1);
    x_fit = linspace(min(validResults.Sham_mu), max(validResults.Sham_mu), 100);
    plot(x_fit, polyval(coeffs, x_fit), 'r-', 'LineWidth', 2);
end
xlabel('Baseline Performance (Sham \mu)');
ylabel('Improvement (\Delta\mu = Active - Sham)');
title(sprintf('Baseline Dependency (Mu): r = %.2f, p = %.3f', r_mu, p_mu));
refline(0,0);

%% --- 5. Visualize Sigma Baseline Dependency (Original) ---
[r_sig, p_sig] = corr(validResults.Sham_sigma, validResults.ChangeSigma, 'Type', 'Spearman');
figure('Color', 'w', 'Name', 'Baseline Dependency (Sigma)');
hold on; grid on;
scatter(validResults.Sham_sigma, validResults.ChangeSigma, 70, 'filled', 'MarkerEdgeColor', 'k');
text(validResults.Sham_sigma + 0.0005, validResults.ChangeSigma, validResults.SID, 'FontSize', 8, 'Interpreter', 'none');
if height(validResults) > 2
    coeffs = polyfit(validResults.Sham_sigma, validResults.ChangeSigma, 1);
    x_fit = linspace(min(validResults.Sham_sigma), max(validResults.Sham_sigma), 100);
    plot(x_fit, polyval(coeffs, x_fit), 'b-', 'LineWidth', 2);
end
xlim([0 0.06]);
xlabel('Baseline Performance (Sham \sigma)');
ylabel('Improvement (\Delta\sigma = Active - Sham)');
title(sprintf('Baseline Dependency (Sigma): r = %.2f, p = %.3f', r_sig, p_sig));
refline(0,0);

%% --- 6. Visualize Global Efficiency Baseline Dependency (New) ---
[r_glob, p_glob] = corr(validResults.Sham_GlobEff, validResults.ChangeGlobEff, 'Type', 'Spearman');
figure('Color', 'w', 'Name', 'Baseline Dependency (Global Efficiency)');
hold on; grid on;
% Using a distinct color (Purple/Dark Gray) for Global Efficiency
scatter(validResults.Sham_GlobEff, validResults.ChangeGlobEff, 70, [0.4 0.2 0.6], 'filled', 'MarkerEdgeColor', 'k');
text(validResults.Sham_GlobEff + (max(validResults.Sham_GlobEff)*0.01), ...
     validResults.ChangeGlobEff, validResults.SID, 'FontSize', 8, 'Interpreter', 'none');
if height(validResults) > 2
    coeffs = polyfit(validResults.Sham_GlobEff, validResults.ChangeGlobEff, 1);
    x_fit = linspace(min(validResults.Sham_GlobEff), max(validResults.Sham_GlobEff), 100);
    plot(x_fit, polyval(coeffs, x_fit), 'k-', 'LineWidth', 2);
end
xlabel('Baseline Global Efficiency (\mu \times \sigma)');
ylabel('Improvement (\Delta Global Efficiency)');
title(sprintf('Global Efficiency: r = %.2f, p = %.3f', r_glob, p_glob));
refline(0,0);

%% --- 7. Median Split Analyses (Mu, Sigma, & Global) ---
fprintf('\n--- Median Split Analysis (Mu) ---\n');
medianBaseMu = median(validResults.Sham_mu);
[~, p_med_mu] = ttest2(validResults.Change(validResults.Sham_mu <= medianBaseMu), ...
                       validResults.Change(validResults.Sham_mu > medianBaseMu));
fprintf('T-test p-value (High vs Low Mu): %.4f\n', p_med_mu);

fprintf('\n--- Median Split Analysis (Sigma) ---\n');
medianBaseSigma = median(validResults.Sham_sigma);
[~, p_med_sig] = ttest2(validResults.ChangeSigma(validResults.Sham_sigma <= medianBaseSigma), ...
                        validResults.ChangeSigma(validResults.Sham_sigma > medianBaseSigma));
fprintf('T-test p-value (High vs Low Sigma): %.4f\n', p_med_sig);

fprintf('\n--- Median Split Analysis (Global Efficiency) ---\n');
medianBaseGlob = median(validResults.Sham_GlobEff);
[~, p_med_glob] = ttest2(validResults.ChangeGlobEff(validResults.Sham_GlobEff <= medianBaseGlob), ...
                         validResults.ChangeGlobEff(validResults.Sham_GlobEff > medianBaseGlob));
fprintf('T-test p-value (High vs Low Global): %.4f\n', p_med_glob);
%% --- 8. Visualize Preparation Efficiency (AUC) Baseline Dependency ---
% Calculate Spearman correlation for AUC
[r_auc, p_auc] = corr(validResults.PEI_Baseline, validResults.PEI_Change, 'Type', 'Spearman');

figure('Color', 'w', 'Name', 'Preparation Efficiency Analysis (AUC)');
hold on; grid on;

% Color mapping: using a "Success" oriented color (Emerald Green)
scatter(validResults.PEI_Baseline, validResults.PEI_Change, 80, [0.1, 0.6, 0.4], 'filled', 'MarkerEdgeColor', 'k');

% Add participant labels
text(validResults.PEI_Baseline + (max(validResults.PEI_Baseline)*0.005), ...
     validResults.PEI_Change, validResults.SID, 'FontSize', 8, 'Interpreter', 'none');

% Fit regression line
if height(validResults) > 2
    coeffs = polyfit(validResults.PEI_Baseline, validResults.PEI_Change, 1);
    x_fit = linspace(min(validResults.PEI_Baseline), max(validResults.PEI_Baseline), 100);
    plot(x_fit, polyval(coeffs, x_fit), 'k-', 'LineWidth', 2);
end

% Labels and Formatting
xlabel('Baseline Preparation Efficiency (Sham AUC)');
ylabel('Improvement (\Delta AUC = Active - Sham)');
title(sprintf('Global Preparation Efficiency: r = %.2f, p = %.3f', r_auc, p_auc));

% Zero line indicates no effect
refline(0,0);

% Final Median Split for AUC
fprintf('\n--- Median Split Analysis (AUC Efficiency) ---\n');
medianBaseAUC = median(validResults.PEI_Baseline);
[~, p_med_auc] = ttest2(validResults.PEI_Change(validResults.PEI_Baseline <= medianBaseAUC), ...
                        validResults.PEI_Change(validResults.PEI_Baseline > medianBaseAUC));
fprintf('T-test p-value (High vs Low Baseline AUC): %.4f\n', p_med_auc);

%% --- 9. Visualize Asymptote Baseline Dependency (New) ---
[r_glob, p_glob] = corr(validResults.Sham_asym, validResults.ChangeAsym, 'Type', 'Spearman');
figure('Color', 'w', 'Name', 'Baseline Dependency (Global Efficiency)');
hold on; grid on;
% Using a distinct color (Purple/Dark Gray) for Global Efficiency
scatter(validResults.Sham_asym, validResults.ChangeAsym, 70, [0.4 0.2 0.6], 'filled', 'MarkerEdgeColor', 'k');
text(validResults.Sham_asym + (max(validResults.Sham_asym)*0.01), ...
     validResults.ChangeAsym, validResults.SID, 'FontSize', 8, 'Interpreter', 'none');
if height(validResults) > 2
    coeffs = polyfit(validResults.Sham_asym, validResults.ChangeAsym, 1);
    x_fit = linspace(min(validResults.Sham_asym), max(validResults.Sham_asym), 100);
    plot(x_fit, polyval(coeffs, x_fit), 'k-', 'LineWidth', 2);
end
xlabel('Baseline Global Efficiency (\mu \times \sigma)');
ylabel('Improvement (\Delta Global Efficiency)');
title(sprintf('Global Efficiency: r = %.2f, p = %.3f', r_glob, p_glob));
refline(0,0);

%% --- 9. Visualize Asymptote Sigma change Dependency (New) ---
[r_glob, p_glob] = corr(validResults.ChangeSigma, validResults.ChangeAsym, 'Type', 'Spearman');
figure('Color', 'w', 'Name', 'Change Sigma (Noise) versus Change Asymptote (Attention)');
hold on; grid on;
% Using a distinct color (Purple/Dark Gray) for Global Efficiency
scatter(validResults.ChangeSigma, validResults.ChangeAsym, 70, [0.4 0.2 0.6], 'filled', 'MarkerEdgeColor', 'k');
text(validResults.ChangeSigma + (max(validResults.ChangeSigma)*0.01), ...
     validResults.ChangeAsym, validResults.SID, 'FontSize', 8, 'Interpreter', 'none');
if height(validResults) > 2
    coeffs = polyfit(validResults.ChangeSigma, validResults.ChangeAsym, 1);
    x_fit = linspace(min(validResults.ChangeSigma), max(validResults.ChangeSigma), 100);
    plot(x_fit, polyval(coeffs, x_fit), 'k-', 'LineWidth', 2);
end
xlabel('Sigma Change');
ylabel('Asymptote Change');
title(sprintf('Global Efficiency: r = %.2f, p = %.3f', r_glob, p_glob));
refline(0,0);