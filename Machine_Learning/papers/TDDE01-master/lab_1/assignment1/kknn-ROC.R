library(kknn)

ROC=function(Y, Yfit, p) {
  m=length(p)
  TPR=numeric(m)
  FPR=numeric(m)
  
  for(i in 1:m) {
    t=table(Yfit>p[i], Y)
    
    TP = t[2,2]
    FP = t[2,1]
    TN = t[1,1]
    FN = t[1,2]
    
    TPR[i]=TP/(TP+FN)
    FPR[i]=FP/(FP+TN)
  }
  plot(FPR, TPR, xlim=c(0,1), ylim=c(0,1))
  abline(a=0,b=1)
}

p <- seq(from=0.05, to=0.95, by=0.05)

model <- kknn(Spam ~ ., train, test, k = 5)
fit <- fitted(model)
result <- floor(fit + 0.5)

ROC(test$Spam, fit, p)

CM <- table(factor(test$Spam, labels=c("Actual not spam", "Actual spam")),
            factor(result, labels=c("Pred not spam", "Pred spam")));

print(CM)
