%% --- AGGREGATION & CORRELATION SCRIPT: Baseline Dependency (Mu & Sigma) ---
% This script mimics the style of opening a .mat file per subject.


participants = {'FRTP01', 'FRTP02', 'FRTP03', 'FRTP04', 'FRTP05', 'FRTP06', ...
                'FRTP07', 'FRTP09', 'FRTP11', 'FRTP12', ...
                'FRTP14', 'FRTP16', 'FRTP17', 'FRTP18', 'FRTP20', 'FRTP21',  ...
                'FRTP22','FRTP23', 'FRTP24', 'FRTP25', 'FRTP26', 'FRTP27', 'FRTP28', ...
                'FRTP29', 'FRTP30', 'FRTP31', 'FRTP32', 'FRTP33'};

base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\Reward_experiment'; 
kinarm_delay = 0.0; % 20ms adjustment

% Preallocate results table (Added columns for Sigma)
results = table('Size', [length(participants) 7], ...
    'VariableTypes', {'string', 'double', 'double', 'double', 'double', 'double', 'double'}, ...
    'VariableNames', {'SID', 'no_reward_mu', 'reward_mu', 'Change', 'no_reward_sigma', 'reward_sigma', 'ChangeSigma'});

fprintf('Processing Individual Fits per Participant...\n');

% --- 1. Loop through each participant ---
for p = 1:length(participants)
    pat = participants{p};
    disp(['Processing: ' pat]);
    
    % Load participant data
    pat_file = fullfile(base_dir, pat, [pat, '_reward_reach_data_analyzed.mat']);
    if ~exist(pat_file, 'file'); continue; end
    L = load(pat_file);
    data = L.reward_data; 
    
    % Define condition mapping based on .mat structure
    NO_REWARD = 3; REWARD = 4;                
    
    % Prepare data (Filtering out NaNs and catch trials if not already done)
    valid_trials = (~isnan(data.eff_pt) & data.catch_trial == 1);                
    data = data(valid_trials, :);
    
    % --- 2. Fit Individual Models ---
    % Fit no_reward Condition
    idx_no_reward = (data.reward_type == NO_REWARD);
    norew_model = fit_model((data.eff_pt(idx_no_reward) / 1000) - kinarm_delay, data.success(idx_no_reward));
    results.no_reward_mu(p) = norew_model.paramsOpt(1); % Mu
    results.no_reward_sigma(p) = norew_model.paramsOpt(2); % Sigma

    
    % Fit reward Condition
    idx_reward = (data.reward_type == REWARD);
    rew_model = fit_model((data.eff_pt(idx_reward) / 1000) - kinarm_delay, data.success(idx_reward));
    results.reward_mu(p) = rew_model.paramsOpt(1); % Mu
    results.reward_sigma(p) = rew_model.paramsOpt(2); % Sigma

    
    results.SID(p) = pat;
end

% --- 3. Calculate Change and Correlate ---
results.Change = results.reward_mu - results.no_reward_mu;
results.ChangeSigma = results.reward_sigma - results.no_reward_sigma;

% Remove subjects with failed fits (NaN)
validResults = results(~isnan(results.no_reward_mu) & ~isnan(results.reward_mu), :);

% Spearman Correlation (Robust to outliers)
[r, p_val] = corr(validResults.no_reward_mu, validResults.Change, 'Type', 'Spearman');
fprintf('Correlation (Mu): r = %.2f, p = %.3f\n', r, p_val);

%% --- 4. Visualize Mu Baseline Dependency ---

figure('Color', 'w', 'Name', 'Baseline Dependency (Mu)');
hold on; grid on;

% Plot points
scatter(validResults.no_reward_mu, validResults.Change, 70, 'filled', 'MarkerEdgeColor', 'k');

% --- ADD SID TAGS ---
text(validResults.no_reward_mu + 0.005, validResults.Change, validResults.SID, ...
    'FontSize', 8, 'Interpreter', 'none');

% --- ADD REGRESSION LINE ---
if height(validResults) > 2
    % Linear fit for visualization
    coeffs = polyfit(validResults.no_reward_mu, validResults.Change, 1);
    x_fit = linspace(min(validResults.no_reward_mu), max(validResults.no_reward_mu), 100);
    y_fit = polyval(coeffs, x_fit);
    plot(x_fit, y_fit, 'r-', 'LineWidth', 2);
end

xlabel('Baseline Performance (no_reward \mu)');
ylabel('Improvement (\Delta\mu = reward - no_reward)');
title(sprintf('Baseline Dependency: r = %.2f, p = %.3f', r, p_val));
refline(0,0); % Line at 0 change

%% --- Visualize Sigma Baseline Dependency ---
figure('Color', 'w', 'Name', 'Baseline Dependency (Sigma)');
hold on; grid on;

% Plot points
scatter(validResults.no_reward_sigma, validResults.ChangeSigma, 70, 'filled', 'MarkerEdgeColor', 'k');

% --- ADD SID TAGS ---
text(validResults.no_reward_sigma + 0.0005, validResults.ChangeSigma, validResults.SID, ...
    'FontSize', 8, 'Interpreter', 'none');

% --- ADD REGRESSION LINE ---
if height(validResults) > 2
    % Linear fit for visualization
    coeffs = polyfit(validResults.no_reward_sigma, validResults.ChangeSigma, 1);
    x_fit = linspace(min(validResults.no_reward_sigma), max(validResults.no_reward_sigma), 100);
    y_fit = polyval(coeffs, x_fit);
    plot(x_fit, y_fit, 'r-', 'LineWidth', 2);
end

xlabel('Baseline Performance (no_reward \sigma)');
ylabel('Improvement (\Delta\sigma = reward - no_reward)');
% Use spearman to match the calculation in the script
[r_sig, p_sig] = corr(validResults.no_reward_sigma, validResults.ChangeSigma, 'Type', 'Spearman');
title(sprintf('Baseline Dependency (Sigma): r = %.2f, p = %.3f', r_sig, p_sig));
refline(0,0); % Line at 0 change

% --- 5. MEDIAN SPLIT TEST (MU) ---

fprintf('\n--- Median Split Analysis (Mu) ---\n');

% Calculate median baseline
medianBaseMu = median(validResults.no_reward_mu);

% Assign groups
validResults.GroupMu = repmat({'Low'}, height(validResults), 1);
validResults.GroupMu(validResults.no_reward_mu > medianBaseMu) = {'High'};

% Perform t-test between groups for change in mu
lowGroupMu = validResults.Change(strcmp(validResults.GroupMu, 'Low'));
highGroupMu = validResults.Change(strcmp(validResults.GroupMu, 'High'));

[h_mu, p_median_mu, ci_mu, stats_mu] = ttest2(lowGroupMu, highGroupMu);

fprintf('Median Baseline Mu: %.4f\n', medianBaseMu);
fprintf('Mean Change (Low Group): %.4f\n', mean(lowGroupMu));
fprintf('Mean Change (High Group): %.4f\n', mean(highGroupMu));
fprintf('T-test p-value (High vs Low Mu): %.4f\n', p_median_mu);

% --- 6. MEDIAN SPLIT TEST (SIGMA) ---

fprintf('\n--- Median Split Analysis (Sigma) ---\n');

% Calculate median baseline sigma
medianBaseSigma = median(validResults.no_reward_sigma);

% Assign groups
validResults.GroupSigma = repmat({'Low'}, height(validResults), 1);
validResults.GroupSigma(validResults.no_reward_sigma > medianBaseSigma) = {'High'};

% Perform t-test between groups for change in sigma
lowGroupSigma = validResults.ChangeSigma(strcmp(validResults.GroupSigma, 'Low'));
highGroupSigma = validResults.ChangeSigma(strcmp(validResults.GroupSigma, 'High'));

[h_sigma, p_median_sigma, ci_sigma, stats_sigma] = ttest2(lowGroupSigma, highGroupSigma);

fprintf('Median Baseline Sigma: %.4f\n', medianBaseSigma);
fprintf('Mean Change Sigma (Low Group): %.4f\n', mean(lowGroupSigma));
fprintf('Mean Change Sigma (High Group): %.4f\n', mean(highGroupSigma));
fprintf('T-test p-value (High vs Low Sigma): %.4f\n', p_median_sigma);