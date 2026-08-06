%% AGGREGATION SCRIPT: Reward Vigor vs. Decision Speed (Flipped Sign)
participants = {'FRTP01', 'FRTP02', 'FRTP03', 'FRTP04', 'FRTP05', 'FRTP06', 'FRTP07', ...
    'FRTP09', 'FRTP11', 'FRTP12', 'FRTP14', 'FRTP16', 'FRTP17', ...
    'FRTP18', 'FRTP20', 'FRTP21', 'FRTP22', 'FRTP23', 'FRTP24', 'FRTP25', 'FRTP26', ...
    'FRTP27', 'FRTP28', 'FRTP29', 'FRTP30', 'FRTP31', 'FRTP32'}; 
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\reward_experiment'; 
kinarm_delay = 0.02;

% Initialize table
subject_deltas = table('Size', [length(participants), 3], ...
                       'VariableTypes', {'string', 'double', 'double'}, ...
                       'VariableNames', {'SID', 'Delta_Vel', 'Delta_Speed'}); 

fprintf('Processing Reward Correlation Data (Speed Transformation)...\n');

for p = 1:length(participants)
    pat = participants{p};
    
    % Construct file path
    pat_file = fullfile(base_dir, pat, [pat, '_reward_reach_data_analyzed.mat']); 
    if ~exist(pat_file, 'file'); continue; end
    load(pat_file);
    
    % --- 1. Calculate Velocity Delta ---
    REWARD_NO = 3;
    REWARD_YES = 4;
   
    filter_outliers = @(data) data(data >= (quantile(data, 0.25) - 1.5*iqr(data)) & ...
                                   data <= (quantile(data, 0.75) + 1.5*iqr(data)));
    
    vel_no_raw  = reward_data.peak_tanVel(reward_data.reward_type == REWARD_NO);
    vel_rew_raw = reward_data.peak_tanVel(reward_data.reward_type == REWARD_YES);
    
    vel_no_clean  = filter_outliers(vel_no_raw(~isnan(vel_no_raw)));
    vel_rew_clean = filter_outliers(vel_rew_raw(~isnan(vel_rew_raw)));
    
    delta_vel = (mean(vel_rew_clean)/mean(vel_no_clean)) - (mean(vel_no_clean)/mean(vel_no_clean)); 
    
    % --- 2. Calculate Decision Speed Delta (Speed = -Delta Mu) ---
    VT = (reward_data.eff_pt / 1000) - kinarm_delay;
    hit = reward_data.success;
    idx_no = (reward_data.reward_type == REWARD_NO & reward_data.catch_trial == 1);
    idx_rew = (reward_data.reward_type == REWARD_YES & reward_data.catch_trial == 1);
    
    try
        m_no = fit_model(VT(idx_no), hit(idx_no));
        m_rew = fit_model(VT(idx_rew), hit(idx_rew));
        
        % SPEED TRANSFORMATION:
        % Positive value means Reward was FASTER than No-Reward
        delta_speed = -(m_rew.paramsOpt(1) - m_no.paramsOpt(1)); 
        
    catch ME
        delta_speed = NaN;
    end
    
    % --- 3. Store Results ---
    subject_deltas.SID(p) = pat;
    subject_deltas.Delta_Vel(p) = delta_vel;
    subject_deltas.Delta_Speed(p) = delta_speed; 
end

% Cleanup
subject_deltas(isnan(subject_deltas.Delta_Speed), :) = [];

%% --- 4. Plotting ---
X = subject_deltas.Delta_Speed * 1000; % Convert to ms for readability
Y = subject_deltas.Delta_Vel;

figure('Color', 'w', 'Name', 'Reward Vigor Correlation');
hold on; grid on; box on;

% Scatter Plot
scatter(X, Y, 120, 'filled', 'MarkerFaceColor', [0.85 0.33 0.1], 'MarkerEdgeColor', 'k');
text(X + (0.02*range(X)), Y, subject_deltas.SID, 'FontSize', 9, 'FontWeight', 'bold');

% Regression Line
if length(X) > 2
    poly_fit = polyfit(X, Y, 1);
    x_range = [min(X)-5, max(X)+5];
    plot(x_range, polyval(poly_fit, x_range), 'b-', 'LineWidth', 2);
    [R, Pval] = corr(X, Y, 'Type', 'Pearson');
end

% Zero Reference Lines
xline(0, '--k', 'HandleVisibility', 'off');
yline(0, '--k', 'HandleVisibility', 'off');

% Labels
xlabel('\Delta Decision Speed (ms) [Right = Faster Onset under Reward]');
ylabel('\Delta Movement Vigor (\Delta Peak Velocity)');
title({['Reward Correlation: Decision Speed vs. Movement Vigor'], ...
       sprintf('(r = %.2f, p = %.3f, N = %d)', R, Pval, length(X))});

% Interpretation Note
if R > 0 && Pval < 0.05
    fprintf('Significant Positive Correlation: Participants who increased decision speed the most also showed the largest increase in movement velocity.\n');
end