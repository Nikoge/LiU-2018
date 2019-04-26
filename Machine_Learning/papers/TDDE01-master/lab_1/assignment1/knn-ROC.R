#imports the excel-file
library(readxl)
data <- read_excel("TDDE01/lab1/assignment1/spambase.xlsx")

#divide data into training and test
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]

knearest_multik=function(data,t,k,newdata) {
  n1=dim(data)[1]
  n2=dim(newdata)[1]
  p=dim(data)[2]
  Prob=numeric(n2)
  
  X=as.matrix(data[,-p])
  Xclass=as.matrix(data[,p])
  X=X/matrix(sqrt(rowSums(X^2)), nrow=n1, ncol=p-1)
  
  Y=as.matrix(newdata[,-p])
  Yclass=as.matrix(newdata[,p])
  Y=Y/matrix(sqrt(rowSums(Y^2)), nrow=n1, ncol=p-1)
  
  TPR=numeric(length(t))
  FPR=numeric(length(t))
  
  C <- X %*% t(Y)
  D <- 1-C
  
  for(j in 1:length(t)){
    class <- rep(0,n2)
    
    for (i in 1:n2 ){
      Drow <- D[,i, drop=F]
      Drow <- cbind(Drow,Xclass)
      Drow <- Drow[order(Drow[,1]),]
      Drow <- head(Drow,k)
      
      class[i] <- if(sum(Drow[,2])/k > t[j]) 1 else 0; 
    }
    CM <- table(factor(Yclass, labels=c("Actual not spam", "Actual spam")),
                factor(class, labels=c("Pred not spam", "Pred spam")));
    
    TPR[j] = (CM[2,2] / (CM[2,2] + CM[2,1]))
    FPR[j] = (CM[1,2] / (CM[1,2] + CM[1,1]))
  }
  
  plot(FPR, TPR, xlim=c(0,1), ylim=c(0,1))
  abline(a=0,b=1)
  
  return(CM)
}

p <- seq(from=0.05, to=0.95, by=0.05)

knearest_multik(train, p, 5, test)