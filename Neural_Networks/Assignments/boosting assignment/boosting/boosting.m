load faces;
load nonfaces;

faces = double(faces); 
nonfaces = double(nonfaces);

 numbersOfClassifiers = 40;
 nbrHaarFeatures = 25;
 nbrTrainExamples = 1000;
 nbrTestExamples = 2000;
 
 
figure(1)
 colormap gray
 for k =1:25
 subplot (5 ,5 ,k), imagesc ( faces (: ,: ,10*k)), axis image , axis off
 end
 
 figure(2)
 colormap gray
 for k =1:25
 subplot (5 ,5 ,k), imagesc ( nonfaces (: ,: ,10*k)), axis image , axis off
 end
 
 % Generate Haar feature masks
haarFeatureMasks = GenerateHaarFeatureMasks ( nbrHaarFeatures );
 figure (3)
 colormap gray
 for k = 1:25
 subplot (5 ,5 ,k),imagesc ( haarFeatureMasks (: ,:,k) ,[-1 2])
 axis image , axis off
 end
 
 % Create a training data set with a number of training data examples
 % from each class . Non - faces = class label y=-1, faces = class label y=1
 trainImages = cat(3, faces (: ,: ,1: nbrTrainExamples ),nonfaces (: ,: ,1:nbrTrainExamples ));
 testImages = cat(3, faces (: ,: ,1001:3000) ,nonfaces (: ,: ,1001:3000) );
 xTrain = ExtractHaarFeatures ( trainImages , haarFeatureMasks );
 xTest = ExtractHaarFeatures ( testImages , haarFeatureMasks );
 yTrain = [ones(1, nbrTrainExamples), -ones(1,nbrTrainExamples)];
 yTest = [ones(1 ,nbrTestExamples),-ones(1,nbrTestExamples)];

 d = ones(1 ,2* nbrTrainExamples ) ./(2* nbrTrainExamples );

 %k: feature index
 %ii: threshold index

 for jj =1: numbersOfClassifiers
 % find the best classfier
 [feature(jj), threshold(jj), sign(jj), errorBest] = findBestClassifier(xTrain , yTrain , d);
 % test that classifier
 cTrain = classify(xTrain , feature(jj), threshold(jj), sign(jj));
 % calculate the alpha for that classifier
 alpha(jj) = (1/2) * log((1 - errorBest)/errorBest);
 % calculate the weights
 d = d.* exp (- alpha(jj)*(yTrain .* cTrain));
 % renomalise the weights
 d = d/(sum(d));
 % calculate the accuracy depending on the number of classifiers used
 accuracyTest(jj) = strongClassifierAcc(xTest, yTest, feature, threshold, sign, alpha)
 accuracyTrain(jj) = strongClassifierAcc(xTrain, yTrain, feature, threshold, sign, alpha)
end

figure ();
plot(accuracyTrain);
hold on;
plot(accuracyTest);
title("Accuracy of Train(blue) vs. Test(red) dataset");
hold off ;