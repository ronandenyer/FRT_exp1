%% Basic example

a = [1,2,3,4,5];   %mean =3
b = [0,0,0,-4,2];   %mean = -0.4
testa = ComputeMean(a);
testb = ComputeMean(b);

c = [nan, 2, 3, 4];   %mean = 3
testc = ComputeMean(c);

%% Compute the mean of each column of a matrix
%create a matrix
%matrix = [['trial1' 'trial2' 'trial3' 'trial4']; [1 2 3 4]; [5 6 7 8]];
matrix = [[1 2 3 4]; [1 2 3 4]; [5 6 7 8]];

m = size(matrix,2); %number of col of the matrix
means = zeros(1,m);  %initiate the vector in which the means values will be stored

%loop on every column of the matrix
for i = 1:m
    col = matrix(:,i); %extract the column
    means(i) = ComputeMean(col);
    disp('here')
end
    