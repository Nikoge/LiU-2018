library(fastICA)
library(pls)
data = read.csv2(file = "TDDE01/lab2/assignment4/NIRSpectra.csv", header=T, sep=";")

Viscosity = data$Viscosity
data$Viscosity = c()
res = prcomp(data)
set.seed(12345)

screeplot(res)

# PCA analysis
q1 = function() {
  lambda = res$sdev^2
  lambda
  
  sprintf("%2.3f", lambda/sum(lambda)*100)
  
  U = res$rotation
  head(U)
  
  plot(res$x[,1], res$x[,2], ylim=c(-0.12,0.10))
}

# trace plots
q2 = function() {
  U = res$rotation
  plot(U[,1], ylim=c(-0.1,0.4), col="blue", type="l")
  lines(U[,2], col="red")
}

# ICA
q3 = function() {
  a = fastICA(data, 2, alg.typ="parallel", fun="logcosh", alpha=1, method="R", row.norm=F, maxit=200, tol=0.0001, verbose=T)
  Wp = a$K %*% a$W
  
  plot(Wp[,1], ylim=c(-13,2), col="blue", type="l")
  lines(Wp[,2], col="red")
  
  plot(a$S[,1], a$S[,2])
}

# PCR
q4 = function() {
  pcr.fit = pcr(Viscosity ~ ., data=data, validation = "CV")
  summary(pcr.fit)
  validationplot(pcr.fit, val.type="MSEP")
}

#q1()
#q2()
#q3()
#q4()
