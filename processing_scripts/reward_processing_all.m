%% Run all processing scripts for all participants
pats = {'FRTP01', 'FRTP02', 'FRTP03', 'FRTP04', 'FRTP05', 'FRTP06', ...
                'FRTP07', 'FRTP09', 'FRTP11', 'FRTP12', ...
                'FRTP14', 'FRTP16', 'FRTP17', 'FRTP18', 'FRTP20', 'FRTP21',  ...
                'FRTP22','FRTP23', 'FRTP24', 'FRTP25', 'FRTP26', 'FRTP27', 'FRTP28', ...
                'FRTP29', 'FRTP30', 'FRTP31', 'FRTP32', 'FRTP33'};
%pats = {'LEA'};
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\Reward_experiment'; 

for a = 1:length(pats)
    pat = pats{a};
    disp(pat);
    %filtration_loop_reward;
    % free_compactify_raw_data;
    reward_compactify_raw_data;
    reward_reach_analysis;
end

tvns_processing_all;
% %%
% figure('Color', 'w', 'Position', [100, 100, 900, 500]);
% hold on
% 
% % Define RGB colors [R G B]
% color_no_reward = [0.5 0.5 0.5];  % Type 3: No Reward (Gray)
% color_reward    = [0.8 0.2 0.2];  % Type 4: Reward (Red)
% 
% % 1. Plot individual trial traces in the background (fainter alpha = 0.08)
% for i = 1:height(Compact_all)
%     r_type = Compact_all.reward_type(i);
% 
%     if r_type == 3
%         plot(Compact_all.force_z{i}, ...
%             'Color', [color_no_reward, 0.08], ... % Faint background overlay
%             'LineWidth', 0.4, ...
%             'HandleVisibility', 'off');
%     elseif r_type == 4
%         plot(Compact_all.force_z{i}, ...
%             'Color', [color_reward, 0.08], ...
%             'LineWidth', 0.4, ...
%             'HandleVisibility', 'off');
%     end
% end
% 
% % 2. Extract indices for each condition
% idx_no_reward = find(Compact_all.reward_type == 3);
% idx_reward    = find(Compact_all.reward_type == 4);
% 
% % 3. Calculate and plot Grand Mean for No Reward (Type 3)
% if ~isempty(idx_no_reward)
%     % Filter out any corrupt trials with fewer than 1000 points
%     valid_no_reward = idx_no_reward(cellfun(@numel, Compact_all.force_z(idx_no_reward)) >= 1000);
% 
%     if ~isempty(valid_no_reward)
%         % Find the common minimum length across valid trials
%         min_len_3 = min(cellfun(@numel, Compact_all.force_z(valid_no_reward)));
% 
%         % Truncate all trials to min_len_3 and stack into a matrix [Trials x TimePoints]
%         mat_no_reward = cell2mat(cellfun(@(x) reshape(x(1:min_len_3), 1, []), ...
%             Compact_all.force_z(valid_no_reward), 'UniformOutput', false));
% 
%         mean_no_reward = mean(mat_no_reward, 1, 'omitnan');
% 
%         plot(mean_no_reward, ...
%             'Color', color_no_reward, ...
%             'LineWidth', 2.5, ...
%             'DisplayName', 'No Reward (3) Mean');
%     end
% end
% 
% % 4. Calculate and plot Grand Mean for Reward (Type 4)
% if ~isempty(idx_reward)
%     % Filter out any corrupt trials with fewer than 1000 points
%     valid_reward = idx_reward(cellfun(@numel, Compact_all.force_z(idx_reward)) >= 1000);
% 
%     if ~isempty(valid_reward)
%         % Find the common minimum length across valid trials
%         min_len_4 = min(cellfun(@numel, Compact_all.force_z(valid_reward)));
% 
%         % Truncate all trials to min_len_4 and stack into a matrix [Trials x TimePoints]
%         mat_reward = cell2mat(cellfun(@(x) reshape(x(1:min_len_4), 1, []), ...
%             Compact_all.force_z(valid_reward), 'UniformOutput', false));
% 
%         mean_reward = mean(mat_reward, 1, 'omitnan');
% 
%         plot(mean_reward, ...
%             'Color', color_reward, ...
%             'LineWidth', 2.5, ...
%             'DisplayName', 'Reward (4) Mean');
%     end
% end