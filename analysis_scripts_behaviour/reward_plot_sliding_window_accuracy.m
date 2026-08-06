function reward_plot_sliding_window_accuracy(RT, hit, Reward_Condition)
    % Inputs:
    % RT: Reaction times (automatically detects s or ms)
    % hit: 1 for hit, 0 for miss
    % Reward_Condition: 3 for No-Reward, 4 for Reward
    
    % --- 0. Unit Auto-Detect ---
    if mean(RT, 'omitnan') > 5 
        RT = RT / 1000; % Convert ms to s if values are large
    end

    % --- 1. Parameters ---
    window_width = 0.100; % 100 ms window
    step_size = 0.002;    % 2 ms steps (slightly faster processing)
    time_axis = 0:step_size:0.6; % 0 to 600 ms
    
    noreward_acc = nan(size(time_axis));
    noreward_sem = nan(size(time_axis));
    reward_acc   = nan(size(time_axis));
    reward_sem   = nan(size(time_axis));
    
    % --- 2. Sliding Window Loop ---
    for i = 1:length(time_axis)
        t = time_axis(i);
        win_min = t - (window_width/2);
        win_max = t + (window_width/2);
        
        % No-Reward (Condition 3)
        idx_no = (Reward_Condition == 3) & (RT >= win_min) & (RT <= win_max);
        if sum(idx_no) > 3
            p = mean(hit(idx_no));
            noreward_acc(i) = p;
            noreward_sem(i) = sqrt((p*(1-p))/sum(idx_no));
        end
        
        % Reward (Condition 4)
        idx_rew = (Reward_Condition == 4) & (RT >= win_min) & (RT <= win_max);
        if sum(idx_rew) > 3
            p = mean(hit(idx_rew));
            reward_acc(i) = p;
            reward_sem(i) = sqrt((p*(1-p))/sum(idx_rew));
        end
    end
    
    % --- 3. Plotting ---
    figure('Color', 'w', 'Name', 'Reward Sliding Window'); hold on; grid on; box on;
    
    % Shaded regions
    plot_shaded_error(time_axis, noreward_acc, noreward_sem, [0.4 0.4 0.4]); % Gray
    plot_shaded_error(time_axis, reward_acc, reward_sem, [0.85 0.33 0.10]); % Orange/Red for Reward
    
    % Mean lines
    h1 = plot(time_axis, noreward_acc, 'Color', [0.2 0.2 0.2], 'LineWidth', 2.5);
    h2 = plot(time_axis, reward_acc, 'Color', [0.64 0.08 0.08], 'LineWidth', 2.5);
    
    xlabel('Reaction Time (s)');
    ylabel('Hit Rate (Proportion)');
    title('Empirical Speed-Accuracy Trade-off (100ms Sliding Window)');
    legend([h1, h2], {'No Reward', 'Reward'}, 'Location', 'southeast');
    ylim([0.2 1.05]); xlim([0 0.55]);
end

function plot_shaded_error(x, y, err, color)
    valid = ~isnan(y) & ~isnan(err);
    if isempty(find(valid, 1)), return; end
    xv = x(valid); yv = y(valid); ev = err(valid);
    patch([xv, fliplr(xv)], [(yv + ev), fliplr(yv - ev)], color, ...
          'FaceAlpha', 0.15, 'EdgeColor', 'none', 'HandleVisibility', 'off');
end