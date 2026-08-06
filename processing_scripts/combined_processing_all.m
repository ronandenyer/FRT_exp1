%% Run all processing scripts for all participants
pats = {'FRTC02', 'FRTC05'};
base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\Combined_experiment'; 

for a = 1:length(pats)
    pat = pats{a};
    disp(pat);
    filtration_loop_combined;
    combined_compactify_raw_data;
    combined_reach_analysis;
end
