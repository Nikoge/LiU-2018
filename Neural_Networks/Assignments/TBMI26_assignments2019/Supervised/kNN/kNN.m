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

labelsOut  = zeros(size(X,2),1);
classes = unique(Lt);
numClasses = length(classes);
sizeDataTrain=size(Xt,1);
sizeDataTest=size(X,1);

for i=1:sizeDataTest

    disp(i)
	
	% Difference of feature vector calculation
    difference=zeros(1,sizeDataTrain);
    for j=1:sizeDataTrain
        difference(j)=sum((Xt(j,:)-X(i,:)).^2);
    end
	
	%sort difference vector
    [~,sortedIndex] = sort(difference,'ascend'); 
     
    for j=1:k
        labels(j)=label_train(sortedIndex(j),1);
    end
    lbl=mode(single(labels));

end
end


