library(readxl)
data <- read_excel("TDDE01/lab1/assignment1/spambase.xlsx")

#divide data into training and test
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]

knearest=function(data,k,newdata) {
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
  
  class = numeric(n2)
  
  C <- X %*% t(Y)
  D <- 1-C
  
  for (i in 1:n2 ){
    Drow <- D[,i, drop=F]
    Drow <- cbind(Drow,Xclass)
    Drow <- Drow[order(Drow[,1]),]
    Drow <- head(Drow,k)
    
    #classifies
    class[i] <- if(sum(Drow[,2] > 0)/k > 0.5) 1 else 0; 
  }
  
  CM <- table(factor(Yclass, labels=c("Actual not spam", "Actual spam")),
              factor(class, labels=c("Pred not spam", "Pred spam")));
  
  #print classification rate and CM
  print(1-(CM[1,1]+CM[2,2])/sum(CM))
  print(CM)
}

knearest(train, 1, train)
knearest(train, 5, train)
knearest(train, 1, test)
knearest(train, 5, test)
