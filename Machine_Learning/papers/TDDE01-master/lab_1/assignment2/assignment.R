library(readxl)
data <- read_excel("TDDE01/lab1/assignment2/machines.xlsx")

theta <- seq(from=0.00, to=3, by=0.01)

loglik <- function(x, theta) {
  ll <- numeric(length(theta))
  
  for(i in 1:(length(theta))){
    p <- theta[i]*exp((-theta[i]*x))
    ll[i] <- (log(prod(p))/dim(x)[1])
  }
  
  return(list(LogLik=ll, theta=theta))
}

loglik_b <- function(x, theta) {
  ll <- numeric(length(theta))
  
  for(i in 1:(length(theta))){
    p <- theta[i]*exp((-theta[i]*x))
    pre <- 10*exp(1)**(-theta[i]*10)
    ll[i] <- (log(prod(p)*pre)/dim(x)[1])
  }
  
  return(list(LogLik=ll, theta=theta))
}

result.allData <- loglik(data, theta)
result.firstSix <- loglik(head(data,6), theta)
result.bayesian <- loglik_b(data, theta)

rand <- rexp(50,rate=1.13)

#set.seed(12345)
#hist(rand)
#datam <- as.matrix(data)
#hist(datam)

plot(result.allData$theta, result.allData$LogLik, xlab="Theta", ylab="Log Likelihood", xlim=c(0,3), ylim=c(-5,0), col="blue")
points(result.firstSix$theta, result.firstSix$LogLik, col="red")
points(result.bayesian$theta, result.bayesian$LogLik, col="green")

#View(result.allData)
#View(result.firstSix)
#View(cbind(result.bayesian$LogLik, result.bayesian$theta))