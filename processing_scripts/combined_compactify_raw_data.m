%% Define Participant detailspat = 'ALAN';
%pat = 'FRTC01';
%base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\Combined_experiment'; 
pat_dir = strcat(base_dir, '\', pat);

%% Loop through each block of trials and create simplified table with key data

num_blocks = 8;
num_days = 2;
fs = 1000;
origin = [0, 0.25];
Compact_all = table();
for d = 1:num_days
    for i = 1:num_blocks
        
        
        block_file = strcat(pat_dir, '\', pat, '_reward_tvns_day', string(d), '_block', string(i), '.mat');
        block_data = load(block_file);
    
        block_data_filtered = block_data.block_data_filtered;
        num_trials = numel(block_data_filtered);
        Compact = table();
        Compact.SID = repmat(pat, num_trials, 1);
        Compact.trial_num = transpose(1:num_trials);
        Compact.prep_time = block_data_filtered(1).TP_TABLE.prep_time(1:num_trials);
        Compact.catch_trial = block_data_filtered(1).TP_TABLE.catch_trial(1:num_trials);
        Compact.reward_type = block_data_filtered(1).TP_TABLE.reward_type(1:num_trials);
        Compact.stim_type = block_data_filtered(1).TP_TABLE.stimulation_type(1:num_trials);
        Compact.block_num = repmat(i, num_trials, 1);
        Compact.day_num = repmat(d, num_trials, 1);
        Compact.end_target = transpose(block_data_filtered(1).TP_TABLE.End_target(1:num_trials));
        Compact.RotX = cell(num_trials,1);
        Compact.RotY = cell(num_trials,1);
    
        for k = 1:num_trials
        disp(k);
            tone_1_idx = find(strcmp(block_data_filtered(k).EVENTS.LABELS  , 'PREP_SOUND1'));
            tone_1_time = block_data_filtered(k).EVENTS.TIMES(tone_1_idx);
            tone_3_time = round((tone_1_time + 1)*fs); % events are given in seconds, so transform to milliseconds here
                                                % also adding one second as 2
                                                % tones = 1 second
            Compact.HandX{k} = block_data_filtered(k).Right_HandX(tone_3_time:tone_3_time + 2500);
            Compact.HandY{k} = block_data_filtered(k).Right_HandY(tone_3_time:tone_3_time + 2500);
            Compact.HandXVel{k} = block_data_filtered(k).Right_HandXVel(tone_3_time:tone_3_time + 2500);
            Compact.HandYVel{k} = block_data_filtered(k).Right_HandYVel(tone_3_time:tone_3_time + 2500);
            try
                end_target_idx = find(strcmp(block_data_filtered(k).EVENTS.LABELS  , 'TARGET_ON'));
                end_target_time = block_data_filtered(k).EVENTS.TIMES(end_target_idx);
    
                prep_time_event_code = (end_target_time*1000) - tone_3_time;
                Compact.prep_time_event_code(k) = round(prep_time_event_code);
    
                % assess whether a hit was registered
                hit_target = find(strcmp(block_data_filtered(k).EVENTS.LABELS  , 'TARGET_HIT'));
                
                if isempty(hit_target)
                    Compact.target_hit_event(k) = 0;
                else
                    Compact.target_hit_event(k) = 1;
                end
    
            catch
                Compact.prep_time_event_code(k) = NaN;
            end
            % Determine if movement timing met the "Good Timing" window criteria
            try
                good_timing = find(strcmp(block_data_filtered(k).EVENTS.LABELS, 'GOOD_TIMING'));
                Compact.good_timing(k) = ~isempty(good_timing);
            catch
                Compact.good_timing(k) = NaN;
            end
                
            % Final Trial Success: Must hit correct target AND have correct timing
            Compact.feedback_success(k) = (Compact.target_hit_event(k) == 1 && Compact.good_timing(k) == 1);
             % Now rotate all position data as if they had to go to upper target
            % Extract original position vectors
            x = Compact.HandX{k};
            y = Compact.HandY{k};
        
            % --- Step 1: translate so that (0,20) → (0,0) ---
            x_t = x - origin(1);   % this is just x
            y_t = y - origin(2);   % shift downward by 0.2
        
            % --- Step 2: rotate based on end_target ---
            t = Compact.end_target(k);
        
            switch t
                case 2   % no rotation
                    xRot = x_t;
                    yRot = y_t;
        
                case 3   % 180 degrees
                    xRot = -x_t;
                    yRot = -y_t;
        
                case 4   % 90 degrees anti-clockwise
                    xRot = -y_t;
                    yRot =  x_t;
        
                case 5   % 90 degrees clockwise
                    xRot =  y_t;
                    yRot = -x_t;
        
                otherwise
                    error("Unknown rotation code: %d", t);
            end
        
            % Store results
            Compact.RotX{k} = xRot;
            Compact.RotY{k} = yRot;
    
            % Now rotate all velocity as if they had to go to upper target
            % Extract original position vectors
            xvel = Compact.HandXVel{k};
            yvel = Compact.HandYVel{k};
        
            
            % --- Step 2: rotate based on end_target ---
            t = Compact.end_target(k);
        
            switch t
                case 2   % no rotation
                    xVelRot = xvel;
                    yVelRot = yvel;
        
                case 3   % 180 degrees
                    xVelRot = -xvel;
                    yVelRot = -yvel;
        
                case 4   % 90 degrees anti-clockwise
                    xVelRot = -yvel;
                    yVelRot =  xvel;
        
                case 5   % 90 degrees clockwise
                    xVelRot =  yvel;
                    yVelRot = -xvel;
        
                otherwise
                    error("Unknown rotation code: %d", t);
            end
        
            % Store results
            Compact.RotXVel{k} = xVelRot;
            Compact.RotYVel{k} = yVelRot;
    
            % --- Calculate tangential velocity ---
            Compact.TanVel{k} = sqrt(xVelRot.^2 + yVelRot.^2);
    
            % --- Calculate velocity angle (radians) ---
            Compact.VelAngle{k} = rad2deg(atan2(Compact.HandYVel{k}, Compact.HandXVel{k}));
    
            Compact.VelAngle_rot{k} = rad2deg(atan2(yVelRot, xVelRot));
    
            % Pupil analysis
            fs = 1000; %sample rate
            pre_window = 4000; % time in ms before prep sound 1 to use as window
            post_window = 4000;
            start_idx = int32((tone_1_time*fs) - pre_window);
            end_idx = int32((tone_1_time*fs) + post_window);
    
            eye_raw = block_data_filtered(k).Gaze_PupilArea(start_idx:end_idx);
             
            % The Vectorized Way
            eye_raw(eye_raw < 0) = NaN;
            % Calculate percentage
            nan_percentage = sum(isnan(eye_raw))/length(eye_raw);
            
            Compact.pupil_nan_percent(k) = nan_percentage;
    
            try
                % smooth pupil data
                % smooth_window = 200;
                % 
                eye_smooth = fillmissing(eye_raw,'linear'); %smoothdata(fillmissing(eye_raw,'linear'),'movmean',smooth_window);
        
                % Filter Low pass 3 Hz
        
                cutoff_frequency = 4; % in Hz
                filter_order = 4;
                
                [b,a] = butter(filter_order, cutoff_frequency/(fs/2),'low');
                
                eye_lp = filtfilt(b,a,eye_smooth);
                
                % Normalize by baseline (first 2 seconds of the window)
                baseline_indices = 1000:2000;
                baseline_mean_eye = mean(eye_lp(baseline_indices)); 
                Compact.pupil_size{k} = eye_lp;
                Compact.pupil_size_normalized{k} = eye_lp - baseline_mean_eye;
                Compact.pupil_size_mean{k} = baseline_mean_eye;
                
                phasic_indices = 4000:5000;
                pre_phasic_indices = 3000:4000;
                Compact.pupil_size_pre_phasic_mean{k} = mean(Compact.pupil_size{k}(pre_phasic_indices));
                Compact.pupil_size_pre_phasic_mean_norm{k} = mean(Compact.pupil_size_normalized{k}(pre_phasic_indices));
    
                Compact.pupil_size_phasic_mean_norm{k} = mean(Compact.pupil_size_normalized{k}(phasic_indices));
                Compact.pupil_size_phasic_mean{k} = mean(Compact.pupil_size{k}(phasic_indices));
    
                % --- New: Extract Rate of Change Metrics ---
                % --- Robust Rate of Change Metrics ---
                
                pupil_window = Compact.pupil_size_normalized{k}(phasic_indices);
                
                % Compute local derivatives scaled by sampling rate (units/second)
                pupil_gradient = gradient(pupil_window) * fs;
                
                % Robust Peak Dilation Velocity: Mean of the top 5% fastest slopes
                sorted_gradients = sort(pupil_gradient, 'descend');
                num_points_5pct = round(0.05 * length(sorted_gradients));
                
                Compact.pupil_size_phasic_slope{k} = (pupil_window(end) - pupil_window(1))/length(pupil_window);
                [Compact.pupil_size_max{k}, Compact.pupil_size_max_ind{k}] = max(pupil_window);
                % --------------------------------------
    
                % Reject trials with physically impossible (negative) pupil sizes
                if any(Compact.pupil_size{k} < 0)
                    Compact.pupil_size{k} = NaN;
                    Compact.pupil_size_normalized{k} = NaN;
                    Compact.pupil_size_mean{k} = NaN;
                    Compact.pupil_size_pre_phasic_mean{k} = NaN;
                    Compact.pupil_size_pre_phasic_mean_norm{k} = NaN;
                    Compact.pupil_size_phasic_mean_norm{k} = NaN;
                    Compact.pupil_size_phasic_mean{k} = NaN;
                    Compact.pupil_size_phasic_slope{k} = NaN;
                    Compact.pupil_size_max{k} = NaN;
                    Compact.pupil_size_max_ind{k} = NaN;
                    
                end
            catch
                Compact.pupil_size{k} = NaN;
                Compact.pupil_size_normalized{k} = NaN;
                Compact.pupil_size_mean{k} = NaN;
                Compact.pupil_size_pre_phasic_mean{k} = NaN;
                Compact.pupil_size_pre_phasic_mean_norm{k} = NaN;
                Compact.pupil_size_phasic_mean_norm{k} = NaN;
                Compact.pupil_size_phasic_mean{k} = NaN;
                Compact.pupil_size_phasic_slope{k} = NaN;
                Compact.pupil_size_max{k} = NaN;
                Compact.pupil_size_max_ind{k} = NaN;
            end
            
        end
    
       
        Compact_all = vertcat(Compact_all, Compact);
    
        
    end
end

%% save data

save_filename = strcat(pat, '_combined_reach_data_compact.mat');
save_path = strcat(pat_dir, '\', save_filename);
save(save_path, 'Compact_all');
%% check rotation
% trial = 11;
% hold on
% plot(Compact.HandX{trial}, Compact.HandY{trial}-0.25);
% plot(Compact.RotX{trial}, Compact.RotY{trial});
% xlim([-0.1, 0.1])
% ylim([-0.1, 0.1])
% hold off
% 
% %% check rotation velocity
% 
% trial = 2;
% hold on
% plot(Compact.HandXVel{trial}, Compact.HandYVel{trial});
% plot(Compact.RotXVel{trial}, Compact.RotYVel{trial});
% xlim([-2, 2])
% ylim([-2, 2])
% hold off