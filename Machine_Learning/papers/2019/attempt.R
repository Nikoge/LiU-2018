#2019-01-16

### library


library("purrr")
library("dplyr")
library("glmnet")


##2. scale Mortailty

### scaling the data without mortaility
data <- read.csv("Influenza.csv")
data_scaled <- data %>% select(-c("Mortality")) %>% scale() %>% cbind(Mortality = data$Mortality) %>% as.data.frame()

### 50-50 split
n=nrow(data_scaled)
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data_scaled[id,]
test=data_scaled[-id,]


lambda <- 10^seq(10, -2, length = 100)

X <- train %>% select(-c("Mortality")) %>% as.matrix()
Y <- train %>% select(c("Mortality")) %>% as.matrix()
X_test <- test %>% select(-c("Mortality")) %>% as.matrix()
Y_test <- test %>% select(c("Mortality")) %>% as.vector()


lasso_cv <- cv.glmnet(X,Y, alpha=1, lambda = lambda, type.measure="mse", family = "poisson")
plot(lasso_cv)
min_lambda <- lasso_cv$lambda.min
min_lambda

# best model
best_lasso <- glmnet(X, Y, alpha = 1, family = "poisson", lambda = min_lambda)
best_lasso$beta

# printing the mse
Y_test$predict_y <- predict(best_lasso, newx = X_test, type = "response")
Y_test$MSE <- (Y_test$predict_y - Y_test$Mortality)^2

#alpha value
cat("The beta from the model are:") 
best_lasso$beta
cat("The alpha value is: ", best_lasso$a0, "while the exp value of alpha is: ", exp(best_lasso$a0))
cat("The mean squared error is:", mean(Y_test$MSE))

## 3.

new_data <- data %>% filter(Year > 1994 & Year < 1997)

x <- new_data %>% select(-c("Year")) 
y <- new_data %>% select(c("Year"))
y <- factor(y$Year)

#initialzing the dataframe
p_values <- data.frame(feature = '',P_value = 0,stringsAsFactors = FALSE)

for(i in 1:ncol(x)){
  res = t.test(x[,i]~y, data = new_data, alternative="two.sided", conf.level = 0.95)
  p_values[i,] <- c(colnames(x)[i],res$p.value)
}

manual_BH_df = function(data, fpr) {
  data2 <- data %>% 
    mutate(BH=p.adjust(P_value, "BH")) %>%
    arrange(P_value) %>%
    mutate(j = rank(P_value), m = n()) %>%
    mutate(BHmanual_base = j/m) %>%
    mutate(BHmanual_alpha = 0.15 * j / m,
           BHsignif_alpha = BHmanual_alpha <= fpr)
  
  return(data2)
}

manual_BH_df(data=p_values, fpr=0.05)

manual_BH(original_pval, alpha=0.5)



# function and testing
original_pval <- as.numeric(p_values$P_value)

manual_BH = function(pvals) {
  data.frame(pval = pvals) %>% 
    mutate(BH=p.adjust(pval, "BH")) %>%
    arrange(pval) %>%
    mutate(j = rank(pval), m = n()) %>%
    mutate(BHmanual_base = j/m) %>%
    mutate(BHmanual_alpha = 0.15 * j / m,
           BHsignif_alpha = pval <= BHmanual_alpha)
}

manual_BH(original_pval, alpha=0.5)
