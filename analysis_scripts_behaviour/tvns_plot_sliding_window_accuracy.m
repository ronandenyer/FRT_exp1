function tvns_plot_sliding_window_accuracy(RT, hit, Stim_Condition)
    % Inputs:
    % RT: Reaction times in seconds
    % hit: 1 for hit, 0 for miss
    % Stim_Condition: 1 for Sham, 2 for tVNS
    
    % --- 1. Parameters ---
    window_width = 0.100; % 100 ms window
    step_size = 0.001;    % 1 ms steps
    time_axis = 0:step_size:0.5; % 0 to 500 ms
    
    % Initialize storage
    sham_acc = nan(size(time_axis));
    sham_sem = nan(size(time_axis));
    tvns_acc = nan(size(time_axis));
    tvns_sem = nan(size(time_axis));
    
    % --- 2. Sliding Window Loop ---
    for i = 1:length(time_axis)
        t = time_axis(i);
        
        % Define window boundaries
        win_min = t - (window_width/2);
        win_max = t + (window_width/2);
        
        % Filter trials within window for Sham
        idx_sham = (Stim_Condition == 1) & (RT >= win_min) & (RT <= win_max);
        if sum(idx_sham) > 5 % Only calculate if we have enough trials
            p = mean(hit(idx_sham));
            n = sum(idx_sham);
            sham_acc(i) = p;
            sham_sem(i) = sqrt((p*(1-p))/n);
        end
        
        % Filter trials within window for tVNS
        idx_tvns = (Stim_Condition == 2) & (RT >= win_min) & (RT <= win_max);
        if sum(idx_tvns) > 5
            p = mean(hit(idx_tvns));
            n = sum(idx_tvns);
            tvns_acc(i) = p;
            tvns_sem(i) = sqrt((p*(1-p))/n);
        end
    end
    
    % --- 3. Plotting ---
    figure('Color', 'w'); hold on; grid on; box on;
    
    % Helper function for shaded error bars
    plot_shaded_error(time_axis, sham_acc, sham_sem, [0.4 0.4 0.4]); % Gray for Sham
    plot_shaded_error(time_axis, tvns_acc, tvns_sem, [0.0 0.45 0.74]); % Blue for tVNS
    
    % Plot the mean lines on top
    h1 = plot(time_axis, sham_acc, 'Color', [0.2 0.2 0.2], 'LineWidth', 2);
    h2 = plot(time_axis, tvns_acc, 'Color', [0.0 0.30 0.60], 'LineWidth', 2);
    
    xlabel('Reaction Time (s)');
    ylabel('Empirical Accuracy (Hit Rate)');
    title(['Sliding Window Accuracy (Width: ', num2str(window_width*1000), 'ms)']);
    legend([h1, h2], {'Sham', 'tVNS'}, 'Location', 'southeast');
    ylim([0 1.1]); xlim([0 0.5]);
end

function plot_shaded_error(x, y, err, color)
    % Removes NaNs so the fill function doesn't break
    valid = ~isnan(y) & ~isnan(err);
    x = x(valid); y = y(valid); err = err(valid);
    
    if isempty(x), return; end
    
    X_patch = [x, fliplr(x)];
    Y_patch = [(y + err), fliplr(y - err)];
    
    fill(X_patch, Y_patch, color, 'FaceAlpha', 0.2, 'EdgeColor', 'none', 'HandleVisibility', 'off');
end