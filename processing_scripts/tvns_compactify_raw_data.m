%% --- Participant and Directory Setup ---
% Define ID and paths for data retrieval
%pat = 'LEA'; 
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\tVNS_experiment'; 
pat_dir = fullfile(base_dir, pat);

%% --- Global Parameters & Initialization ---
num_blocks = 4;                 % 4 blocks per condition
stim_conditions = ["sham", "tvns"];
fs = 1000;                      % Sampling frequency (Hz)
origin = [0, 0.25];             % Reaching starting point for translation
Compact_all = table();          % Master table to store processed data across all blocks

%% --- Main Processing Loop ---
% Iterate through experimental conditions (Sham vs. tVNS) and blocks
for j = 1:length(stim_conditions)
    for i = 1:num_blocks
        
        % Load raw block data
        block_file = fullfile(pat_dir, strcat(pat, '_', stim_conditions(j) ,'_block', string(i), '.mat'));
        block_data = load(block_file);
        block_data_filtered = block_data.block_data_filtered;
        num_trials = numel(block_data_filtered);
        
        % Initialize temporary table for the current block
        Compact = table();
        Compact.SID = repmat(pat, num_trials, 1);
        Compact.trial_num = transpose(1:num_trials);
        Compact.prep_time = block_data_filtered(1).TP_TABLE.prep_time(1:num_trials);
        Compact.catch_trial = block_data_filtered(1).TP_TABLE.catch_trial(1:num_trials);
        Compact.stim_type = block_data_filtered(1).TP_TABLE.stimulation_type(1:num_trials);
        Compact.block_num = repmat(i, num_trials, 1);
        Compact.end_target = transpose(block_data_filtered(1).TP_TABLE.End_target(1:num_trials));
        Compact.RotX = cell(num_trials,1);
        Compact.RotY = cell(num_trials,1);
    
        %% --- Per-Trial Analysis ---
        for k = 1:num_trials
            % 1. TIME ALIGNMENT
            % Locate the first preparation tone to anchor the trial timeline
            tone_1_idx = find(strcmp(block_data_filtered(k).EVENTS.LABELS, 'PREP_SOUND1'));
            tone_1_time = block_data_filtered(k).EVENTS.TIMES(tone_1_idx);
            
            % Movement onset (Tone 3) occurs 1s (1000ms) after Tone 1
            tone_3_time = round((tone_1_time + 1) * fs); 
            
            % 2. EXTRACT REACH TRAJECTORIES (Raw)
            % Extract 2.5 seconds of hand position and velocity starting from movement cue
            time_range = tone_3_time : tone_3_time + 2500;
            Compact.HandX{k} = block_data_filtered(k).Right_HandX(time_range);
            Compact.HandY{k} = block_data_filtered(k).Right_HandY(time_range);
            Compact.HandXVel{k} = block_data_filtered(k).Right_HandXVel(time_range);
            Compact.HandYVel{k} = block_data_filtered(k).Right_HandYVel(time_range);
            
            % 3. TASK PERFORMANCE METRICS
            try
                % Calculate Preparation Time (Target Onset relative to Movement Cue)
                end_target_idx = find(strcmp(block_data_filtered(k).EVENTS.LABELS, 'TARGET_ON'));
                end_target_time = block_data_filtered(k).EVENTS.TIMES(end_target_idx);
                Compact.prep_time_event_code(k) = round((end_target_time * 1000) - tone_3_time);
    
                % Determine if the physical target was triggered
                hit_target = find(strcmp(block_data_filtered(k).EVENTS.LABELS, 'TARGET_HIT'));
                Compact.target_hit_event(k) = ~isempty(hit_target);
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
    
            % 4. TRAJECTORY NORMALIZATION (Rotation)
            % We rotate all movements to a common "North" (Upper Target) frame for analysis
            x = Compact.HandX{k}; y = Compact.HandY{k};
            x_t = x - origin(1); y_t = y - origin(2); % Translate to origin
            
            t = Compact.end_target(k);
            switch t
                case 2, xRot =  x_t; yRot =  y_t; % North (No rotation)
                case 3, xRot = -x_t; yRot = -y_t; % South (180 deg)
                case 4, xRot = -y_t; yRot =  x_t; % West (90 deg CCW)
                case 5, xRot =  y_t; yRot = -x_t; % East (90 deg CW)
                otherwise, error("Unknown rotation code: %d", t);
            end
            Compact.RotX{k} = xRot; Compact.RotY{k} = yRot;
    
            % Rotate Velocity Vectors accordingly
            xv = Compact.HandXVel{k}; yv = Compact.HandYVel{k};
            switch t
                case 2, xVelRot =  xv; yVelRot =  yv;
                case 3, xVelRot = -xv; yVelRot = -yv;
                case 4, xVelRot = -yv; yVelRot =  xv;
                case 5, xVelRot =  yv; yVelRot = -xv;
            end
            Compact.RotXVel{k} = xVelRot; Compact.RotYVel{k} = yVelRot;
    
            % 5. DERIVED KINEMATICS
            Compact.TanVel{k} = sqrt(xVelRot.^2 + yVelRot.^2); % Resultant Speed
            Compact.VelAngle{k} = rad2deg(atan2(yv, xv));       % Raw Heading Angle
            Compact.VelAngle_rot{k} = rad2deg(atan2(yVelRot, xVelRot)); % Normalized Heading
    
            % 6. PUPIL DATA ANALYSIS
            % Extract pupil area from 4s before Tone 1 to 8s after
            pre_window = 4000; post_window = 8000;
            start_idx = int32((tone_1_time * fs) - pre_window);
            end_idx = int32((tone_1_time * fs) + post_window);
    
            eye_raw = block_data_filtered(k).Gaze_PupilArea(start_idx:end_idx);
            eye_raw(eye_raw < 0) = NaN; % Handle signal loss/blinks as NaNs
            
            Compact.pupil_nan_percent(k) = sum(isnan(eye_raw)) / length(eye_raw);
            
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
        
        % Append current block to master table
        Compact_all = vertcat(Compact_all, Compact);
    end
end

%% --- Export Processed Data ---
save_filename = strcat(pat, '_tvns_reach_data_compact.mat');
save_path = fullfile(pat_dir, save_filename);
save(save_path, 'Compact_all');
disp(['Data processing complete for: ', pat]);