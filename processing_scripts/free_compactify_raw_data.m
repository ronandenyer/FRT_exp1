%% Define Participant details
%pat = 'FRTP21';
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\Reward_experiment'; 
pat_dir = strcat(base_dir, '\', pat);

%% Loop through each block of trials and create simplified table with key data

num_blocks = 4; % twice for tvns/sham
fs = 1000;
origin = [0, 0.25];
rt_threshold = 0.02;  % velocity threshold
Compact_all = table();
        
try       
    block_file = strcat(pat_dir, '\', pat, '_test_v1_100trials.mat');
    block_data = load(block_file);
catch
    block_file = strcat('C:\Users\denyer\OneDrive - UCL\FRT_data\tVNS_experiment', '\', pat, '\', pat, '_test_v1_100trials.mat');
    block_data = load(block_file);
end

block_data_filtered = block_data.block_data_filtered;
num_trials = numel(block_data_filtered);
Compact = table();
Compact.SID = repmat(pat, num_trials, 1);
Compact.RotX = cell(num_trials,1);
Compact.RotY = cell(num_trials,1);

for k = 1:num_trials
    tp_row = block_data_filtered(k).TRIAL.TP_ROW;
    Compact.end_target(k) = block_data_filtered(1).TP_TABLE.End_target(tp_row);
disp(k);
    tone_1_idx = find(strcmp(block_data_filtered(k).EVENTS.LABELS  , 'PREP_SOUND1'));
    tone_1_time = block_data_filtered(k).EVENTS.TIMES(tone_1_idx);
    tone_3_time = round((tone_1_time + 1)*fs); % events are given in seconds, so transform to milliseconds here
                                        % also adding one second as 2
                                        % tones = 1 second
    Compact.HandX{k} = block_data_filtered(k).Right_HandX(tone_3_time:tone_3_time + 1500);
    Compact.HandY{k} = block_data_filtered(k).Right_HandY(tone_3_time:tone_3_time + 1500);
    Compact.HandXVel{k} = block_data_filtered(k).Right_HandXVel(tone_3_time:tone_3_time + 1500);
    Compact.HandYVel{k} = block_data_filtered(k).Right_HandYVel(tone_3_time:tone_3_time + 1500);
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
    Compact.VelAngle{k} = rad2deg(atan2(yVelRot, xVelRot));

    % calculate peak velocity
    Compact.peak_tanVel(k) = max(Compact.TanVel{k}(500:1250));

    % calculate reaction time

    
    % Find first index where velocity exceeds threshold
    try
        idx = find(Compact.TanVel{k} > rt_threshold, 1, 'first');

    % then subtract time cue appeared (500ms) from this
    
        Compact.rt(k) = idx - 500;
    catch
        Compact.rt(k) = NaN;
    end

    
end
    
        




%% save data
Compact = Compact(:, {'SID', 'end_target', 'peak_tanVel', 'rt'});
save_filename = strcat(pat, '_free_response_reach_data_compact.mat');
save_path = strcat(pat_dir, '\', save_filename);
save(save_path, 'Compact');

% also save in tVNS directory for ease of analysis
Compact = Compact(:, {'SID', 'end_target', 'peak_tanVel', 'rt'});
save_filename = strcat(pat, '_free_response_reach_data_compact.mat');
save_path = strcat('C:\Users\denyer\OneDrive - UCL\FRT_data\tVNS_experiment', '\', pat, '\', save_filename);
save(save_path, 'Compact');

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