library(readxl)
library(glmnet)
data <- read_excel("TDDE01/lab1/assignment4/tecator.xlsx")

View(data)

covariates=scale(data[,2:101])
response=scale(data[,102])

lasso.model=cv.glmnet(as.matrix(covariates), response, alpha=1, lambda = seq(0,1,0.001))
View(lasso.model)
print(lasso.model$lambda.min)
coef(lasso.model, s="lambda.min")

plot(lasso.model)