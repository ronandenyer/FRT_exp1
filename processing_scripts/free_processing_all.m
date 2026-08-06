%% Run all processing scripts for all participants
pats = %{'FRTP17'}%{'FRTP01', 'FRTP02', 'FRTP03', 'FRTP04', 'FRTP05', 'FRTP06', 'FRTP07',  'FRTP09', 'FRTP11', 'FRTP12', 'FRTP13', 'ALAN', 'LEA'}; 

base_dir = 'C:\Users\denyer\OneDrive - UCL\FRT_data\tvns_experiment'; 

for a = 1:length(pats)
    pat = pats{a};
    free_compactify_raw_data;
end