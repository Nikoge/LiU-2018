library(readxl)
data <- read_excel("TDDE01/lab1/tecator.xlsx")

#divides the samples
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))

train=data[id,]
test=data[-id,]

#lengths were not equal
test=test[1:dim(train)[1],]

for(i in 1:6) {
  model <- lm(Moisture ~ poly(Protein, i), data=train)
  
  MSEtrain[i] <- mean((train$Moisture - predict(model, train))**2)
  MSEtest[i] <- mean((test$Moisture - predict(model, test))**2)
}

plot(MSEtrain, ylab="MSE", xlab="Degree", ylim=c(31,35), col="blue", type="l")
lines(MSEtest, col="red")