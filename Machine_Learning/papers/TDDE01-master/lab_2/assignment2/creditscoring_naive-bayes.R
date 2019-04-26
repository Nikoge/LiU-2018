library(readxl)
library(e1071)

data = read_excel("TDDE01/lab2/assignment2/creditscoring.xls")
data$good_bad = as.factor(data$good_bad)
n = dim(data)[1]

# splitting
training = data[1:floor((1/2)*n),]
validation = data[(floor((1/2)*n)+1):(floor((3/4)*n)),]
test = data[(floor((3/4)*n)+1):n,]

# fitting
fit = naiveBayes(good_bad ~ ., data = training)

# predictions
pred.train = predict(fit, newdata = training)
pred.test = predict(fit, newdata = test)

# raw predictions
pred2.train = predict(fit, newdata = training, type = "raw")
pred2.test = predict(fit, newdata = test, type = "raw")

# with a lower threshold
res.train = apply(as.matrix(pred2.train[,1]), 1, function(x) ifelse( x > 0.05, "good", "bad"))
res.test = apply(as.matrix(pred2.test[,1]), 1, function(x) ifelse( x > 0.05, "good", "bad"))

mysum = function(real, pred) {
  tab = table(real, pred)
  print(tab)
  print(1-sum(diag(2)*tab)/sum(tab))
}

# confusion matrices
r1=table(training$good_bad, pred.train)
print(r1)
print(1-sum(diag(2)*r1)/sum(r1))

table(test$good_bad, pred.test)

# confusion matrices with loss matrix
table(training$good_bad, res.train)
table(test$good_bad, res.test)