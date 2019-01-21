%% PCA and LDA demo using county data.
% Some code is commented because it only works in Matlab 2017 and later,
% and replaced with equivalent loops.
% Also, the demo contains pause statements. Press any button to continue.

%% Setup
load countrydata
VarNames = {'Population density', ...
            'Proportion of town residents', ...
            'Average lifespan of women', ...
            'Average lifespan of men', ...
            'Ability to read', ...
            'Increase in population', ...
            'Infant mortality', ...
            'Births per 1000', ...
            'Deceases per 1000', ...
            'GNP', ...
            'Nativity/Mortality', ...
            'Avg number of children', ...
            'Population'};

scaledData = countrydata;

% Normalize each variable to range [0,1]
for i = 1:size(scaledData, 1)
   scaledData(i, :) = (scaledData(i, :) - min(scaledData(i, :))) / (max(scaledData(i, :)) - min(scaledData(i, :)));
end
%scaledData = (scaledData - min(scaledData, [], 2)) ./ (max(scaledData, [], 2) - min(scaledData, [], 2));

% Plot original data
figure;
subplot(2,1,1);
imagesc(countrydata);
axis image;
set(gca, 'xTick', 1:size(countrydata, 2));
set(gca, 'xTickLabel', countries);
set(gca, 'xTickLabelRotation', 45);
set(gca, 'yTick', 1:size(countrydata, 1));
set(gca, 'yTickLabel', VarNames);
title('Country data', 'FontSize', 20);
colorbar;

pause;

% Plot scaled data
subplot(2,1,2);
imagesc(scaledData);
axis image;
set(gca, 'xTick', 1:size(countrydata, 2));
set(gca, 'xTickLabel', countries);
set(gca, 'xTickLabelRotation', 45);
set(gca, 'yTick', 1:size(countrydata, 1));
set(gca, 'yTickLabel', VarNames);
title('Country data, scaled to [0,1]', 'FontSize', 20);
colorbar;

N = size(scaledData, 2);

pause;

%% Covariance and correlation

Cov  = zeros(size(scaledData, 1));
Corr = zeros(size(scaledData, 1));

%S = scaledData - mean(scaledData, 2);

for i = 1:size(Cov, 1)
    for j = 1:size(Cov, 2)
        x = scaledData(i,:);
        y = scaledData(j,:);
        
        xm = mean(x);
        ym = mean(y);
        
        Cov(i, j)  = (x-xm) * (y-ym)' / N;
        Corr(i, j) = Cov(i, j) / sqrt((x-xm) * (x-xm)' * (y-ym) * (y-ym)' / N^2);
    end 
end

clf;

% Plot covariance
subplot(1,2,1);
imagesc(Cov);
axis image;
colorbar;
set(gca, 'xTick', 1:size(countrydata, 1));
set(gca, 'xTickLabel', VarNames);
set(gca, 'xTickLabelRotation', 45);
set(gca, 'yTick', 1:size(countrydata, 1));
set(gca, 'yTickLabel', VarNames);
title('Covariance', 'FontSize', 20);

% Plot correlation
subplot(1,2,2);
imagesc(Corr, [-1 1]);
axis image;
colorbar;
set(gca, 'xTick', 1:size(countrydata, 1));
set(gca, 'xTickLabel', VarNames);
set(gca, 'xTickLabelRotation', 45);
set(gca, 'yTick', 1:size(countrydata, 1));
set(gca, 'yTickLabel', VarNames);
title('Correlation', 'FontSize', 20);

pause;

%% Print PCA formulas

clf;

subplot(2,1,1);
text(0.5, 0.5, '$\displaystyle \textrm{max } \sigma^2_{\hat{w}}= \frac{w^TCw}{w^Tw}$','interpreter','latex', 'FontSize', 50, 'HorizontalAlignment', 'Center');
axis off;

pause;
subplot(2,1,2);
text(0.5, 0.5, '$\displaystyle Cw=\sigma^2_{\hat{w}}w$','interpreter','latex', 'FontSize', 50, 'HorizontalAlignment', 'Center');
axis off;

pause;

%% Calculate eigenvalues and vectors

[E, L, I] = sorteig(Cov);

clf;

% Plot actual eigenvalues
subplot(1,3,1);
stem(L, 'LineWidth', 2);
set(gca, 'xtick', 1:size(countrydata, 1));
set(gca, 'xTickLabels', {I});
title('Eigenvalues (sorted)', 'FontSize', 20);
axis square;

pause;

% Plot scaled eigenvalues
subplot(1,3,2);
stem(L / sum(L) * 100, 'LineWidth', 2);
set(gca, 'xTick', 1:size(countrydata, 1));
set(gca, 'xTickLabels', {I});
ylabel('% of information', 'FontSize', 20);
title('Eigenvalues (sorted)', 'FontSize', 20);
axis square;

pause;

% Plot cumulative sum
subplot(1,3,3);
stem(cumsum(L / sum(L) * 100), 'LineWidth', 2);
set(gca, 'xTick', 1:size(countrydata, 1));
set(gca, 'xTickLabels', {I});
ylim([0,100]);
ylabel('% of total information', 'FontSize', 20);
title('Eigenvalues (sorted)', 'FontSize', 20);
axis square;

pause;

%% Principle Components

clf;

hold on;
c = 'rgb';

% Find projections of data onto 3D space
for i = 1:size(countrydata, 2)
   PC1 = E(:,1)' * scaledData(:,i);
   PC2 = E(:,2)' * scaledData(:,i);
   PC3 = E(:,3)' * scaledData(:,i);
   scatter3(PC1, PC2, PC3, 1000, ['.' c(countryclass(i)+1)]);
end
grid on;
xlabel('PC 1');
ylabel('PC 2');
zlabel('PC 3');

pause;

% Remove class 1
clf;
hold on;
for i = 1:size(countrydata, 2)
    if (countryclass(i) == 1)
        continue;
    end
    PC1 = E(:,1)' * scaledData(:,i);
    PC2 = E(:,2)' * scaledData(:,i);
    PC3 = E(:,3)' * scaledData(:,i);
    scatter3(PC1, PC2, PC3, 1000, ['.' c(countryclass(i)+1)]);
end
xlim([-1.5, 1.5]);
ylim([-1.8, 0]);
zlim([-0.6, 0.6]);
grid on;
xlabel('PC 1');
ylabel('PC 2');
zlabel('PC 3');

pause;

% Plot possible separation line (OBS not the optimum, just a random line!)
plot([-1.3, 0], [-0.9, -0.1], 'LineWidth', 2);
hold off;

pause;

%% Print LDA formulas

clf;

subplot(2,1,1);
text(0.5, 0.5, '$\displaystyle \textrm{max } \epsilon(w)=\frac{(\mu_1-\mu_2)^2}{\sigma^2_1+\sigma^2_2}$','interpreter','latex', 'FontSize', 50, 'HorizontalAlignment', 'Center');
axis off;

pause;

subplot(2,1,2);
text(0.5, 0.5, '$\displaystyle w \sim C^{-1}_{tot}(\bar{x}_1-\bar{x}_2)$','interpreter','latex', 'FontSize', 50, 'HorizontalAlignment', 'Center');
axis off;

pause;

%% Do LDA

X1 = scaledData(:, countryclass == 0);
X2 = scaledData(:, countryclass == 2);

N1 = size(X1, 2);
N2 = size(X2, 2);

%C1 = (X1 - mean(X1, 2)) * (X1 - mean(X1, 2))' / N1;
%C2 = (X2 - mean(X2, 2)) * (X2 - mean(X2, 2))' / N2;

C1 = zeros(size(X1, 1));
C2 = zeros(size(X2, 1));

for i = 1:size(C1, 1)
    for j = 1:size(C1, 2)
        x = X1(i,:);
        y = X1(j,:);
        
        xm = mean(x);
        ym = mean(y);
        
        C1(i, j)  = (x-xm) * (y-ym)' / N1;
    end 
end

for i = 1:size(C2, 1)
    for j = 1:size(C2, 2)
        x = X2(i,:);
        y = X2(j,:);
        
        xm = mean(x);
        ym = mean(y);
        
        C2(i, j)  = (x-xm) * (y-ym)' / N2;
    end 
end

Ctot = C1 + C2;
W = Ctot \ (mean(X1, 2) - mean(X2, 2));

clf;

% Plot projection onto w
subplot(1,2,1);
hold on;
plot(W'*X1, 0, 'r.', 'MarkerSize', 30);
plot(W'*X2, 0, 'b.', 'MarkerSize', 30);
title('Projection onto vector obtained by LDA', 'FontSize', 20);
xlabel('W', 'FontSize', 20);
axis square;

pause;

% Separate in y-direction to make it more clear (still projection onto w)
cla;
hold on;
plot(W'*X1, 1:length(X1), 'r.', 'MarkerSize', 30);
plot(W'*X2, 1:length(X2), 'b.', 'MarkerSize', 30);
title('Projection onto vector obtained by LDA', 'FontSize', 20);
xlabel('W', 'FontSize', 20);
ylabel('Country number in cluster', 'FontSize', 20);
axis square;

pause;

% Plot w, shows importance of features for separation
subplot(1,2,2);
stem(W, 'LineWidth', 2);
title('Importance of features', 'FontSize', 20);
set(gca, 'xTick', 1:size(countrydata, 1));
set(gca, 'xTickLabel', VarNames);
set(gca, 'xTickLabelRotation', 45);
axis square;

pause;

% Plot scaled data again to remind of where we started
clf;
imagesc(scaledData);
axis image;
set(gca, 'xTick', 1:size(countrydata, 2));
set(gca, 'xTickLabel', countries);
set(gca, 'xTickLabelRotation', 45);
set(gca, 'yTick', 1:size(countrydata, 1));
set(gca, 'yTickLabel', VarNames);
title('Country data, scaled to [0,1]', 'FontSize', 20);
colorbar;
