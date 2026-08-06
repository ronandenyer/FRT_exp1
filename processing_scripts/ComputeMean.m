function avg = ComputeMean(list)
% Function which compute the mean of a list 
% INPUT = list, the vector of which we want to compute the mean 
% OUTPUT = avg, the average value of the vector list
  
    totalSum = 0; % Initiate the sum
    n = length(list); %Number of element in the vector
    count_nan = 0;
    % Compute the sum of the vector
    for i = 1:n
        if isnan(list(i))
            count_nan = count_nan +1 ;
            disp('nan')
        else
        totalSum = totalSum + list(i);
        end
    end
    
    % Compute the mean 
    avg = totalSum / (n - count_nan);
end

