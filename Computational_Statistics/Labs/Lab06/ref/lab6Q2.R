physical <- read.csv("physical1.csv")

data<-physical

floglik<-function(data, lambda, n){
  2*log(prod(data$X))-(2*n*log(2*lambda))-(1/lambda)*(sum(data$X * (data$Y+(data$Z)/2), na.rm=TRUE))
}

EM<- function(data, lambdak){
  Zobs  <- data$Z[!is.na(data$Z)]
  Zmiss <- data$Z[is.na(data$Z)]
  
  n<- length(data$X)
  r<- length(Zobs)
  
  
  lambdak=100
  
  lambda_fun <-function(lambdak){
    1/(2*n)*(sum(data$X*data$Y)+(sum(data$X[!is.na(data$Z)] * Zobs/2))+(n-r)*lambdak)
  } 
  
  lambda=lambda_fun(lambdak)
  lambdas <- c(lambda)
  
  log_vec<- c()
  log_vec<- floglik(data, lambda, r)
  
  
  k <- 0
  while (abs(lambda-lambdak)> 0.001) {
    
    lambdak <- lambda
    lambda <- lambda_fun(lambdak)
    lambdas <- c(lambdas,lambda)
    
    log_vec <- c(log_vec, floglik(data, lambda, r))
    
    
    k <- k+1
    #lambda <-1/(2*n)(sum(data$X*data$Y)+(sum(data$X[!is.na(data$Z)] * Zobs/2))+(n-r)*lambdak)  
  }
  
  
}