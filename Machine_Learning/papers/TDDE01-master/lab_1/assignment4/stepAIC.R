library(readxl)
library(MASS)
data <- read_excel("TDDE01/lab1/tecator.xlsx")

fit <- lm(Fat ~., data=data[,2:102])
step <- stepAIC(fit, direction="both")
step$anova
summary(step)