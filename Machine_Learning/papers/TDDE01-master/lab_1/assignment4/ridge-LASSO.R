library(readxl)
library(glmnet)
data <- read_excel("TDDE01/lab1/tecator.xlsx", header=F)

covariates=scale(data[,2:101])
response=scale(data[,102])

model.ridge=glmnet(as.matrix(covariates),response, alpha=0)
model.lasso=glmnet(as.matrix(covariates),response, alpha=1)

par(mfrow=c(2,1))
plot(model.ridge, xvar="lambda", label=T)
plot(model.lasso, xvar="lambda", label=T)
par(mfrow=c(1,1))