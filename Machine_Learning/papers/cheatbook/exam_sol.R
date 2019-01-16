# Assignment 2

#### 1.
library(mboost)
bf <- read.csv2("bodyfatregression.csv")
set.seed(1234567890)
m <- blackboost(Bodyfat_percent~Waist_cm+Weight_kg, data = bf)              
                
mstop(m)
cvf <- cv(model.weights(m), type="kfold")
cvm <- cvrisk(m, folds=cvf, grid=1:100)
plot(cvm)
mstop(cvm)


#### 2.
data(spam)
# Nested cross-validation
kfolds <- 2
# Split data
set.seed(12345)
spam_split <- split(spam, 1:kfolds)

fold_cv <- vector("numeric", kfolds)
fold_cv_idx <- 0
for (fold in 1:kfolds) {
  # Select the data
  train <- data.table::rbindlist(spam_split[-fold])
  validation <- spam_split[[fold]]
  best_model <- select_optimal_ksvm(type ~ ., train, 2)
  # Estimate CV of the optimal model using validation set
  y_hat <- predict(best_model, newdata = validation, type = "response")
  y <- validation$type
  fold_cv_idx <- fold_cv_idx + 1
  fold_cv[fold_cv_idx] <- sum(y != y_hat) / length(y)
}

# Average over all CVs
model_selection_cv <- mean(fold_cv)
print(model_selection_cv)


###### 3.
select_optimal_ksvm <- function(formula, data, kfolds, ...) {
  # Fit all models
  models <- list()
  model_idx <- 0
  for (c in c(1, 5)) { # C
    for (kernel in c("rbfdot", "vanilladot")) {
      if (kernel == "vanilladot") {
        # Linear kernel
        model_idx <- model_idx + 1
        models[[model_idx]] <- ksvm(x = formula,data = data,C = c,kernel = kernel,cross = kfolds,...)
      } else {
        # Gaussian kernel (should also consider different width)
        for (width in c(0.01, 0.05)) {
          model_idx <- model_idx + 1
          models[[model_idx]] <- ksvm(x = formula,data = data,C = c,kernel = kernel,kpar = list(sigma = width),
                                      cross = kfolds,...)
  } } } }
  # Select optimal model
  best_model <- NULL
  best_cv <- Inf
  for (model in models) {
    model_cv <- cross(model)
    if (model_cv < best_cv) {
      best_model <- model
      best_cv <- model_cv
    }
  }
  return(best_model)
}
model_returned_to_a_client <- select_optimal_ksvm(type ~ ., spam, 2)