#### TDD exam 2016

### Assignment 1

##### 1.
crx <- read.csv("crx.csv")
n<-dim(crx)[1]
set.seed(12345)
id<-sample(1:n, floor(n*0.8))
train<-crx[id,]
test<-crx[-id,]

library(tree)
tree_ <- tree(as.factor(Class)~., data=train)
plot(tree_)
text(tree_, pretty = 0)

train_new <- train[-2,]
tree_new <- tree(as.factor(Class)~., data=train_new)
plot(tree_new)
text(tree_new, pretty = 0)
# Trees have high variance: a small change in response -> totally different tree

#### 2.
cross_Val <- cv.tree(tree_)
plot(cross_Val$size, cross_Val$dev,xlab = "size",ylab = "dev",type = "b",col = "red")
pruned_mod <- prune.tree(tree_, best = 8) # lowest deviance
tree_data <- dendro_data(pruned_mod)
ggplot(segment(tree_data)) + geom_segment(aes(x = x,y = y,xend = xend,yend = yend)) +
  geom_text(data = label(tree_data),aes(x = x, y = y, label = label),vjust = 1,size = 3) +
  geom_text(data = leaf_label(tree_data),aes(x = x,y = y,label = label),vjust = 0.8,size = 2.5) +
  theme_dendro() + ggtitle("Selected decision tree") + theme(title = element_text(size = 16))

cv_train <- cross_Val
library(ggplot2)
plot_df <- data.frame(size = cv_train$size, deviance = cv_train$dev)
ggplot(plot_df, aes(x = size, y = deviance)) +
  geom_line() +
  scale_x_continuous(breaks = 1:max(plot_df$size)) +
  ylab("Deviance") +
  xlab("Number of leaves") +
  ggtitle("Cross-validation plot")
# Optimal tree
optimal_size <- cv_train$size[which.min(cv_train$dev)]
optimal_tree <- prune.tree(tree_, best = optimal_size)
# Selected variables
selected_vars <- as.character(
  optimal_tree$frame$var[optimal_tree$frame$var != "<leaf>"])
# Optimal tree plot
plot(optimal_tree)
text(optimal_tree)

### 3.

x_train = model.matrix( ~ .-1, train[,-16])
cv_lasso_model <- cv.glmnet(x_train,train[,16], alpha=1, family="binomial")

cv_lasso_model$lambda.min
plot(cv_lasso_model)
cv_lasso_model$lambda.1se


#### 3. for 732A95
model <- gam(Class ~ A3 + A9 + s(A3, k = length(unique(train$A3))),
             family = binomial(), data = train)
# cause A9 is categorical

summary(model)
plot(model)


# 1.4
p_hat <- predict(model,newdata = test,type="response")
error_gam <- E(test[,16],y_gam)

#### 4. 
E <- function(Y, p){
  res <- 0
  for(i in 1:length(Y)){
    temp_res <- (Y[i]*log(p[i]))+((1-Y[i])*log(1-p[i]))
    res <- res+temp_res
  }
  return(res)
}

x_test = model.matrix( ~ .-1, test[,-16])
opt_lasso <- glmnet(x_train,train[,16], alpha=1, family="binomial",lambda = 0.01036912)
Y_lasso <- predict(opt_lasso,newx = x_test, type="response")

error_lasso <- E(test[,16],Y_lasso[,1])


Y_tree <- predict(optimal_tree,newdata=test)

error_tree <- E(test[,16],Y_tree[,2])

# tree model is better since higher (less negative) value. you punish high misclassifications, so 
# so for data where it is important to be really certain, you don't want that.


#### Assignment 2
#### 1.
library(mboost)
bf <- read.csv2("bodyfatregression.csv")
set.seed(1234567890)
m <- blackboost(Bodyfat_percent~Waist_cm+Weight_kg, data=bf)
mstop(m)
cvf <- cv(model.weights(m), type="kfold")
cvm <- cvrisk(m, folds=cvf, grid=1:100)
plot(cvm)
mstop(cvm)
