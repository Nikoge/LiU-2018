% function to perform KNN %

function [labelsOut] = kNN(X, k, Xt, Lt)
%KNN Your implementation of the kNN algorithm
%   Inputs:
%               X  - Features to be classified
%               k  - Number of neighbors
%               Xt - Training features
%               LT - Correct labels of each feature vector [1 2 ...]'
%
%   Output:
%               LabelsOut = Vector with the classified labels

% Make distance map

% pdist2 gives Pairwise distance between two sets of observations
% tranponse of the two matrix
D = pdist2(X',Xt');
    
% The neighbours are in I
% The ~ represents an output that is discarded
% sort takes two arguments sort(A,dim) => sort(D,2) implies sort by row
[~, I] = sort(D, 2);

% Take the mode for the K nearest neighbours
% mode(A,dim) returns the mode of elements along dimension dim. 
%For example, if A is a matrix, then mode(A,2) is a column vector containing the most frequent value of each row
labelsOut = mode(Lt(I(:, 1:k)), 2);
end


