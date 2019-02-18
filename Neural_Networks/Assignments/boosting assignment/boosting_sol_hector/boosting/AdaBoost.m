%% Hyper-parameters
%  You will need to change these. Start with a small number and increase
%  when your algorithm is working.

% Number of randomized Haar-features
nbrHaarFeatures = 100;
% Number of training images, will be evenly split between faces and
% non-faces. (Should be even.)
nbrTrainImages = 1000;
% Number of weak classifiers
nbrWeakClassifiers = 50;

%% Load face and non-face data and plot a few examples
%  Note that the data sets are shuffled each time you run the script.
%  This is to prevent a solution that is tailored to specific images.

load faces;
load nonfaces;
faces = double(faces(:,:,randperm(size(faces,3))));
nonfaces = double(nonfaces(:,:,randperm(size(nonfaces,3))));

figure(1);
colormap gray;
for k=1:25
    subplot(5,5,k), imagesc(faces(:,:,10*k));
    axis image;
    axis off;
end

figure(2);
colormap gray;
for k=1:25
    subplot(5,5,k), imagesc(nonfaces(:,:,10*k));
    axis image;
    axis off;
end

%% Generate Haar feature masks
haarFeatureMasks = GenerateHaarFeatureMasks(nbrHaarFeatures);

figure(3);
colormap gray;
for k = 1:25
    subplot(5,5,k),imagesc(haarFeatureMasks(:,:,k),[-1 2]);
    axis image;
    axis off;
end

%% Create image sets (do NOT modify!)

% Create a training data set with examples from both classes.
% Non-faces = class label y=-1, faces = class label y=1
trainImages = cat(3,faces(:,:,1:nbrTrainImages/2),nonfaces(:,:,1:nbrTrainImages/2));
xTrain = ExtractHaarFeatures(trainImages,haarFeatureMasks);
yTrain = [ones(1,nbrTrainImages/2), -ones(1,nbrTrainImages/2)];

% Create a test data set, using the rest of the faces and non-faces.
testImages  = cat(3,faces(:,:,(nbrTrainImages/2+1):end),...
                    nonfaces(:,:,(nbrTrainImages/2+1):end));
xTest = ExtractHaarFeatures(testImages,haarFeatureMasks);
yTest = [ones(1,size(faces,3)-nbrTrainImages/2), -ones(1,size(nonfaces,3)-nbrTrainImages/2)];

% Variable for the number of test-data.
nbrTestImages = length(yTest);

%% Implement the AdaBoost training here
%  Use your implementation of WeakClassifier and WeakClassifierError
% Initializing the weights D and creating a parameter matrix for
% all of the weak classifiers. It will be of size nbrWeakClassifiers and 3
% the rows will hold 3 values, the optimal threshold, polarity and the
% index of the feature that minimizes the error.
nobs = size(xTrain, 2);
D = (1 / nobs) * ones(nbrWeakClassifiers, nobs);
stump_params = zeros(nbrWeakClassifiers, 3);
alphas = ones(nbrWeakClassifiers);
ah_train = zeros(nbrWeakClassifiers, nobs);
acc_train = zeros(nbrWeakClassifiers, 1);
 
% Training the weak classifiers.
for t = 1:nbrWeakClassifiers
    % Variables that will keep track of the polarity
    % and errors made by the decision stump.
    P = ones(nobs, nbrHaarFeatures);
    errors = ones(nobs, nbrHaarFeatures);
    
    for f = 1:nbrHaarFeatures
        % Getting the feature to evaluate and the threshold.
        x = xTrain(f, :);
        n = 1;
        
        for tau = x
           C = WeakClassifier(tau, P(n, f), x);
           E = WeakClassifierError(C, D(t, :), yTrain);
           
           if E >= 0.5
               P(n, f) = -1;
               C = WeakClassifier(tau, P(n, f), x);
               E = WeakClassifierError(C, D(t, :), yTrain);
           end
           % Updating the errors and the counter (observation index).
           errors(n, f) = E;
           n = n + 1;
           
        end
    end
    
    [idx_threshold, ixd_min_feature] = find(errors==min(min(errors)));
    idx_threshold = idx_threshold(1);
    ixd_min_feature = ixd_min_feature(1);
    
    % Saving the parameters of each stump.
    best_threshold = xTrain(ixd_min_feature, idx_threshold);
    best_polarity = P(idx_threshold, ixd_min_feature);
    
    stump_params(t, :) = [best_threshold, best_polarity, ixd_min_feature];
    
    % Calculating the error and predictions of the best stump
    % for the current bossting round.
    Ct = WeakClassifier(best_threshold, best_polarity, xTrain(ixd_min_feature, :));
    Et = WeakClassifierError(Ct, D(t, :), yTrain);
    
    % Updating alpha and saving it.
    alpha = 0.5 * log((1 - Et) / Et);
    alphas(t) = alpha;
    
    % Updating the weights.
    D(t + 1, :) = D(t, :) .* exp(-alpha .* (yTrain .* Ct));
    D(t + 1, :) = D(t + 1, :) / sum(D(t + 1, :));
    
    % alpha times the prediction.
    ah_train(t, :) = alpha .* Ct;
    
    % Saving the error.
    H = sign(sum(ah_train));
    acc = sum((H == yTrain)) /  length(yTrain);
    acc_train(t, :) = acc;
end

%% Evaluate your strong classifier here
%  You can evaluate on the training data if you want, but you CANNOT use
%  this as a performance metric since it is biased. You MUST use the test
%  data to truly evaluate the strong classifier.

% Calculating the accuracy on the train set.
yhat_train = sign(sum(ah_train));
train_accuracy = sum((yhat_train == yTrain)) /  length(yTrain);
display(train_accuracy);

% Calculating the accuracy on the test set.
ah_test = zeros(nbrWeakClassifiers, size(xTest, 2));
acc_test = zeros(nbrWeakClassifiers, 1);

for t = 1:nbrWeakClassifiers
   tau =  stump_params(t, 1);
   polarity = stump_params(t, 2);
   feature = stump_params(t, 3);
   
   C = WeakClassifier(tau, polarity, xTest(feature, :));
   ah_test(t, :) = alphas(t) .* C;
   
    % Saving the error.
    H = sign(sum(ah_test));
    acc = sum((H == yTest)) /  length(yTest);
    acc_test(t, :) = acc;
   
end

test_accuracy = acc_test(nbrWeakClassifiers, :);
display(test_accuracy);

%% Plot the error of the strong classifier as a function of the number of weak classifiers.
%  Note: you can find this error without re-training with a different
%  number of weak classifiers.
x = 1:nbrWeakClassifiers;
p = plot(x, acc_train, "b", x, acc_test, "r");
title("Train (blue) | Test (red)");


%% Plot some of the misclassified faces and non-faces from the test set
%  Use the subplot command to make nice figures with multiple images.
yhat_test = sign(sum(ah_test));
miss_faces = testImages(:,:,and(yhat_test ~= yTest,yTest ~= -1));
miss_non_faces = testImages(:,:,and(yhat_test ~= yTest,yTest ~= 1));

figure(3);
colormap gray;
for k=1:16
    subplot(4,4,k), imagesc(miss_faces(:,:,k));
    axis image;
    axis off;
end

figure(4);
colormap gray;
for k=1:16
    subplot(4,4,k), imagesc(miss_non_faces(:,:,k));
    axis image;
    axis off;
end

%% Plot your choosen Haar-features
%  Use the subplot command to make nice figures with multiple images.
figure(5);
colormap gray;
for k = 1:25
    f = stump_params(k, 3);
    subplot(5,5,k),imagesc(haarFeatureMasks(:,:,f),[-1 2]);
    axis image;
    axis off;
end
