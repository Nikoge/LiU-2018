library(readxl)
library(tree)

data = read_excel("TDDE01/lab2/assignment2/creditscoring.xls")
data$good_bad = as.factor(data$good_bad)

View(data)

n = dim(data)[1]

# splitting
training = data[1:floor((1/2)*n),]
validation = data[(floor((1/2)*n)+1):(floor((3/4)*n)),]
test = data[(floor((3/4)*n)+1):n,]

# fitting
fit.gini = tree(good_bad ~ ., data = training, split = c("gini"))
fit.dev = tree(good_bad ~ ., data = training, split = c("deviance"))

# predictions
pred.train.gini = predict(fit.gini, newdata=training, type="class")
pred.train.dev = predict(fit.dev, newdata=training, type="class")

pred.test.gini = predict(fit.gini, newdata=test, type="class")
pred.test.dev = predict(fit.dev, newdata=test, type="class")

# confusion matrices
table(training$good_bad, pred.train.gini)
table(training$good_bad, pred.train.dev)

table(test$good_bad, pred.test.gini)
table(test$good_bad, pred.test.dev)

fit.dev
summary(fit.dev)

# finding optimal size
n_leaves = 19

trainScore=rep(0,n_leaves)
testScore=rep(0,n_leaves)

for(i in 2:n_leaves) {
  prunedTree = prune.tree(fit.dev, best = i)
  pred = predict(prunedTree, newdata=validation, type="tree")
  trainScore[i] = deviance(prunedTree)
  testScore[i] = deviance(pred)
}

plot(2:n_leaves, trainScore[2:n_leaves], type="b", col="red", ylim=c(270,570), ylab="train/test scores", xlab="leaves")
points(2:n_leaves, testScore[2:n_leaves], type="b", col="blue")