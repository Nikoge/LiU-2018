%% Hyper-parameters
%  You will need to change these. Start with a small number and increase
%  when your algorithm is working.

% Number of randomized Haar-features
nbrHaarFeatures = 25;
% Number of training images, will be evenly split between faces and
% non-faces. (Should be even.)
nbrTrainImages = 50;
% Number of weak classifiers
nbrWeakClassifiers = 10;

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

n = size(xTrain, 2);
d = ones(1, n) / n;
m = 8;
ts = zeros(m, 1);
ps = zeros(m, 1);
fs = zeros(m, 1);
as = zeros(m, 1);

%%
for i = 1:m
    %disp(i)
    e_min = inf;
    t_min = 0;
    p_min = 0;
    f_min = 0;
    a_min = 0;
    %disp('New Classifier')
    for f = 1:size(xTrain, 1)
        thresholds = xTrain(f,:) + 0.01;
        %disp('New Feature')
        for t = thresholds
            p   = 1;
            h   = WeakClassifier(t, p, xTrain(f,:));
            e_t = WeakClassifierError(h, d, yTrain);

            if e_t > 0.5 
                p = -p;
                e_t = 1 - e_t;
            end

            if e_t < e_min
                e_min = e_t;
                a = log((1 - e_min) / e_min) / 2;
                t_min = t;
                p_min = p;
                f_min = f;
                a_min = a;
                h_min = p*h;
            end
        end    
    end
    
    if e_min == 0.5
        break;
    end
        
    d = d.*exp(-a_min * yTrain .* h_min);
    d(d>0.5) = 0.5;
    d = d ./ sum(d);
    ts(i) = t_min;
    ps(i) = p_min;
    fs(i) = f_min;
    as(i) = a_min;
end
%% Extract test data
nbrTestImages = 1000;

testImages  = cat(3,faces(:,:,(nbrTrainImages+1):(nbrTrainImages+nbrTestImages)),nonfaces(:,:,(nbrTrainImages+1):(nbrTrainImages+nbrTestImages)));
xTest = ExtractHaarFeatures(testImages,haarFeatureMasks);
yTest = [ones(1,nbrTestImages), -ones(1,nbrTestImages)];



%% Evaluate your strong classifier here
%  You can evaluate on the training data if you want, but you CANNOT use
%  this as a performance metric since it is biased. You MUST use the test
%  data to truly evaluate the strong classifier.

n_test = size(yTest,2);
cs = zeros(length(ts), n_test);

for i = 1:size(cs,1)
    size(WeakClassifier(ts(i), ps(i), xTest(fs(i),:)))
    size(as(i))
    cs(i,:) = as(i) * WeakClassifier(ts(i), ps(i), xTest(fs(i),:));
end
 
Classifications = sign(sum(cs,1));

%%
Accuracy = 1 - mean(abs(Classifications - yTest))/2


%% extra

accs = [];
for t = 1:T 
    accs(t) = accuracy(classifiers(:,1:t), xTest, yTest);
end



%% Plot the error of the strong classifier as a function of the number of weak classifiers.
%  Note: you can find this error without re-training with a different
%  number of weak classifiers.

figure(1)
colormap gray
for k=1:25
    subplot(5,5,k), imagesc(testImages(:,:,Classifications(k))), axis image, axis off
end



%% Plot some of the misclassified faces and non-faces from the test set
%  Use the subplot command to make nice figures with multiple images.



%% Plot your choosen Haar-features
%  Use the subplot command to make nice figures with multiple images.

