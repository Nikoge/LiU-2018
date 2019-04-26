library(e1071)
library(readxl)
library(tree)

data = read_excel("TDDE01/lab2/assignment2/creditscoring.xls")
data$good_bad = as.factor(data$good_bad)
n = dim(data)[1]

# splitting
training = data[1:floor((1/2)*n),]
validation = data[(floor((1/2)*n)+1):(floor((3/4)*n)),]
test = data[(floor((3/4)*n)+1):n,]

mysum = function(name, comp, pred) {
  print(name)
  tab = table(comp, pred)
  print(tab)
  print(1-sum(diag(2)*tab)/sum(tab))
}

q2 = function() {
  set.seed(12345)
  # fitting
  fit.gini = tree(good_bad ~ ., data = training, split = c("gini"))
  fit.dev = tree(good_bad ~ ., data = training, split = c("deviance"))
  
  # predictions
  pred.train.gini = predict(fit.gini, newdata=training, type="class")
  pred.train.dev = predict(fit.dev, newdata=training, type="class")
  
  pred.test.gini = predict(fit.gini, newdata=test, type="class")
  pred.test.dev = predict(fit.dev, newdata=test, type="class")
  
  # tables and misclassification
  mysum("train.gini", training$good_bad, pred.train.gini)
  mysum("train.dev", training$good_bad, pred.train.dev)
  
  mysum("test.gini", test$good_bad, pred.test.gini)
  mysum("test.dev", test$good_bad, pred.test.dev)
  
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
  
  # plot of optimal tree
  pruned = prune.tree(fit.dev, best = 8)
  plot(pruned)
  text(pruned, pretty = 0)
}

q3 = function() {
  # fitting
  fit = naiveBayes(good_bad ~ ., data = training)
  
  # predictions
  pred.train = predict(fit, newdata = training)
  pred.test = predict(fit, newdata = test)
  
  # raw predictions
  pred2.train = predict(fit, newdata = training, type = "raw")
  pred2.test = predict(fit, newdata = test, type = "raw")
  
  # with a lower threshold
  res.train = apply(as.matrix(pred2.train[,1]), 1, function(x) ifelse( x < 0.1, "good", "bad"))
  res.test = apply(as.matrix(pred2.test[,1]), 1, function(x) ifelse( x < 0.1, "good", "bad"))
  
  # confusion matrices
  mysum("training", training$good_bad, pred.train)
  mysum("test", test$good_bad, pred.test)
  
  # confusion matrices with loss matrix
  mysum("training lm", training$good_bad, res.train)
  mysum("test lm", test$good_bad, res.test)
}

q2()
