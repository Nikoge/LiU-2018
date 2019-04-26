library(neuralnet)

set.seed(1234567890)
Var <- runif(50, 0, 10)
trva <- data.frame(Var, Sin=sin(Var))
tr <- trva[1:25,] # Training
va <- trva[26:50,] # Validation
winit <- runif(31,-1,1)

for(i in 1:10) { 
  nn <- neuralnet(Sin~Var, data=tr, hidden = 10, threshold = i/1000, startweights = winit)
  pred <- compute(nn, va$Var)$net.result
  MSE[i] <- mean((pred - va$Sin)**2)
}

print(MSE)
print(which.min(MSE))
plot(MSE, xlab="Threshold (i/1000)")

nn <- neuralnet(Sin~Var, data=tr, hidden = 10, threshold = which.min(MSE)/1000, startweights = winit)
plot(nn)

# Plot of the predictions (black dots) and the data (red dots)
plot(trva, col = "red", cex = 0.8)
points(prediction(nn)$rep1)