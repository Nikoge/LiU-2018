function [ feature , threshold , sign , errorBest ] = findBestClassifier (xTrain ,yTrain , d)
nbrTrainExamples = size ( xTrain , 2);
nbrHaarFeatures = size ( xTrain , 1);
errorBest = inf;
for k=1: nbrHaarFeatures
for ii =1: nbrTrainExamples
p =1;
clasForThatThres = ( xTrain (k ,:) >xTrain (k,ii)) .*2 -1;
error = sum (d .*( clasForThatThres ~= yTrain ));
 if error > 0.5
 p= -1;
 error = 1- error ;
 end
 if error < errorBest
 % this are the parameters for the global fest weak classifier
 errorBest = error ;
 % k and ii together define the best cell in the matrix ( least
 % error )
 feature = k;
 threshold = xTrain (k,ii);
 sign = p;
 end
 end
 end
 end