%% --- Participant and Directory Setup ---
% Define ID and paths for data retrieval
%% load data
pat = 'LEA';
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\tVNS_experiment'; 
pat_dir = strcat(base_dir, '\', pat);

tvns_file = strcat(pat_dir, '\', pat, '_tvns_reach_data_compact.mat');
load(tvns_file);
Compact = Compact_all;
%% set parameters

fs = 1000;                      % Sampling frequency (Hz)
origin = [0, 0.25];             % Reaching starting point for translation
trial = 9;
%% 4. TRAJECTORY NORMALIZATION (Rotation)
% We rotate all movements to a common "North" (Upper Target) frame for analysis
x = Compact.HandX{trial}; 
y = Compact.HandY{trial};
x_t = x - origin(1); 
y_t = y - origin(2); % Translate to origin

%% plot this data
% Define the centers and radius
centers = [0, 0.05; 0, -0.05; 0.05, 0; -0.05, 0];
r = 0.005;
colors = [
    1, 0, 0;  % Red
    0, 1, 0   % Red
    1, 0, 0;  % Red
    1, 0, 0   % Green
];

% 3. Plotting
figure(1);
hold on;
plot(x_t, y_t); % Your original data

for i = 1:size(centers, 1)
    viscircles(centers(i,:), r, 'Color', colors(i,:));
end

axis equal tight;
hold off;
%% Rotate trajectories so that target always = 90 degrees


t = Compact.end_target(trial);
switch t
    case 2, xRot =  x_t; yRot =  y_t; % North (No rotation)
    case 3, xRot = -x_t; yRot = -y_t; % South (180 deg)
    case 4, xRot = -y_t; yRot =  x_t; % West (90 deg CCW)
    case 5, xRot =  y_t; yRot = -x_t; % East (90 deg CW)
    otherwise, error("Unknown rotation code: %d", t);
end

%% plot rotated trajectory data

% Define the centers and radius
centers = [0, 0.05; 0, -0.05; 0.05, 0; -0.05, 0];
r = 0.005;
colors = [
    0, 1, 0;  % Green
    1, 0, 0   % Red
    1, 0, 0;  % Red
    1, 0, 0   % Red
];

% 3. Plotting
figure(2);
hold on;
plot(xRot, yRot); % Your original data

for i = 1:size(centers, 1)
    viscircles(centers(i,:), r, 'Color', colors(i,:));
end

axis equal tight;
hold off;
%% Rotate Velocity Vectors accordingly
xv = Compact.HandXVel{trial}; 
yv = Compact.HandYVel{trial};
switch t
    case 2, xVelRot =  xv; yVelRot =  yv;
    case 3, xVelRot = -xv; yVelRot = -yv;
    case 4, xVelRot = -yv; yVelRot =  xv;
    case 5, xVelRot =  yv; yVelRot = -xv;
end



%% 5. GET TANGENTAL VELOCITY and ANGLE
TanVel = sqrt(xVelRot.^2 + yVelRot.^2); % Resultant Speed
VelAngle_rot = rad2deg(atan2(yVelRot, xVelRot)); % Normalized Heading

plot(TanVel);
%%
plot(VelAngle_rot);