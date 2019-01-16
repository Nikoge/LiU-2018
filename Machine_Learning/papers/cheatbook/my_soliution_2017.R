### Exam 732 April 2017

### Assignment 1
#### 1.
crabs <- read.csv("australian-crabs.csv")
ggplot(crabs, aes(x = CW, y = BD, col= species))+
                  geom_point() +
                  scale_colour_manual(name="Species", values = c("blue", "orange"),labels=c("Blue", "Orange")) +
                  xlab("CW") + ylab("BD") + ggtitle("CW vs BD") +
                  theme(plot.title = element_text(hjust = 0.5))

#### 2. 
mod <- naiveBayes(species~ CW+BD, data=crabs, type="raw")
pred_train <- predict(mod, newdata = crabs)
confusion_matrix_train_naive <- table(Observed = crabs$species,Predicted = pred_train)
missclass_rate_train_naive <- (confusion_matrix_train_naive[1,2] +
                                 confusion_matrix_train_naive[2,1])/sum(confusion_matrix_train_naive)


#### 3.
crabs__ <- crabs
crabs__$species <- crabs__$species == "Orange" # true is orange"

pi_k <- c(table(crabs$species)[1]/length(crabs$species), table(crabs$species)[2]/length(crabs$species))
mod <- glm(species ~ CW+BD, data = crabs__, family = binomial())
d <- predict(mod)
Yfit=(d>0)

plot(x=crabs$CW,y=crabs$BD, col=Yfit+1, xlab="CW", ylab="BD", pch = 16)
lines()

predator <- predict(mod, newdata = crabs__, type = "response") > 0.5
predator[predator == FALSE] <- "Blue"
predator[predator == TRUE] <- "Orange"
df <- data.frame(CW =crabs$CW, BD = crabs$BD, pred = predator)
intercept <- summary(mod)$coefficients[1]
sCW <- summary(mod)$coefficients[2]
sBD <- summary(mod)$coefficients[3]
slope <- -sCW / sBD
int <- -intercept / sCW
ggplot(df, aes(x = crabs$CW, y = crabs$BD, color = pred)) +
  geom_point() + xlab("CW") + ylab("BD") +
  geom_abline(slope = slope, intercept = int)

#### 4.
library(stats)
CW_scale <- scale(crabs$CW)
BD_scale <- scale(crabs$BD)

pca_nir <- prcomp(crabs[,7:8], scale = TRUE) 
pca_varexplaind<- pca_nir$sdev^2/sum(pca_nir$sdev^2) #explained variance
pca_eigens <- pca_nir$sdev^2
ggplot() + geom_line(aes(x = 1:length(pca_varexplaind), y = pca_varexplaind)) +
  labs(title="Scree plot for PCA",x="Principal Component", y = "% variance explained")

pcas <- pca_nir$rotation

PC1 <- pcas[,1][1]*CW_scale+pcas[,1][2]*BD_scale
PC2 <- pcas[,2][1]*CW_scale+pcas[,2][2]*BD_scale
#### 5.
df=data.frame(species=crabs$species,PC1=PC1,PC2=PC2)
mod <- naiveBayes(crabs$species~PC1+PC2 ,data=df, type="raw")
pred_train <- predict(mod, newdata = df)
confusion_matrix_train_naive <- table(Observed = crabs$species,Predicted = pred_train)
missclass_rate_train_naive <- (confusion_matrix_train_naive[1,2] +
                                 confusion_matrix_train_naive[2,1])/sum(confusion_matrix_train_naive)

hist(PC1)
plot(PC1, PC2)

### Assignment 2
library(boot)
data_bank <- read.csv2("bank.csv")
modl <- glm(Visitors ~ Time, data = data_bank, poisson(link = "log"))
#only for being able to predict the data and the higher data
#we have to predict for 12,...,13 by 0.05 but making a data for all times
pred_for <- seq(9,13,0.05)
mle <- modl
#generate data function
rng <- function(data, mle){
  #empty data.frame
  data1=data.frame(Time = pred_for, Visitors = pred_for)
  n = length(data1$Visitors)#generate new Price
  X <- coef(mle)
  #making predictions with the model
  lambda <- X[2] * pred_for + X[1]
  #remember the distribution of the resposne
  data1$Visitors <- rpois(n, exp(lambda))
  return(data1)
}

## PI
#do the poisson regression from the data in rng
f1_pi <- function(data1){
  res <- glm(Visitors ~ Time, data = data1, poisson(link = "log")) #fit model
  #predictvalues for all Visitors valuesfrom the original data
  X2 <- coef(res)
  pred <- X2[2] * pred_for + X2[1]
  n <- length(pred_for)
  predictedP = rpois(n, exp(pred)) #poisson because response
  return(predictedP)
}

p_boot <- boot(data_bank, statistic=f1_pi, R=1000, mle=mle,ran.gen=rng, sim="parametric")
ci_boot_pi <- envelope(p_boot)$point
#plotting
matr <- matrix(NA, length(seq(12.05,13,0.05)), 2)
matr[,1] <- seq(12.05,13,0.05)
colnames(matr) <- colnames(data_bank)
dat <- rbind(data_bank, matr)
plot(dat, ylim = c(30, 200))
lines(dat$Time, ci_boot_pi[1,], col = "red")
lines(dat$Time, ci_boot_pi[2,], col = "red")


#### Assignment Ensemble methods
#### 1.
library(mboost)
bf <- read.csv2("bodyfatregression.csv")
set.seed(1234567890)
m <- blackboost(Bodyfat_percent~Waist_cm+Weight_kg, data=bf)
mstop(m)
cvf <- cv(model.weights(m),type="kfold") 
cvm<- cvrisk(m, folds=cvf, grid=1:100)
plot(cvm)
mstop(cvm)

#### 2.
set.seed(12345)
bf <- bf[sample(nrow(bf), replace = FALSE),]
n <- nrow(bf)
ind <- sample(1:n, floor(n*(2/3)),replace = F)
train <- bf[ind,]
test <- bf[-ind,]

m2 <- blackboost(Bodyfat_percent ~ Waist_cm + Weight_kg, data = train,
                 control=boost_control(mstop=mstop(cvm)))
# cvf2 <- cv(model.weights(m2), type = "kfold")
# cvm2 <- cvrisk(m2, folds = cvf2, grid = 1:100)
m2_train <- mean( (predict(m2,train) - train$Bodyfat_percent)^2)
m2_test <- mean( (predict(m2,test) - test$Bodyfat_percent)^2)
cat("SSE for traning:",m2_train,"\n SSE for test:",m2_test)
