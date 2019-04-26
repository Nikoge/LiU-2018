#-----------------------------------------
# Task 3.4
#-----------------------------------------
mle <- tree(EX ~ MET, data = state, control = tree.control(nob = nrow(state), minsize = 8))
mle <- prune.tree(mle, best = 3)

rng <- function(data, mle) {
  data1 <- data.frame(EX = data$EX, MET = data$MET)
  n <- length(data$EX)
  pred <- predict(mle, newdata = data1)
  residual <- data1$EX - pred
  data1$EX <- rnorm(n, pred, sd(residual))
  return(data1)
}
conf.fun <- function(data1) {
  model <- tree(EX ~ MET, data = data1, control = tree.control(nob = nrow(data1), minsize = 8))
  model <- prune.tree(model, best = 3)
  pred <- predict(model, newdata = state)
  return(pred)
}
pred.fun <- function(data1) {
  model <- tree(EX ~ MET, data = data1, control = tree.control(nob = nrow(data1), minsize = 8))
  model <- prune.tree(model, best = 3)
  pred <- predict(model, newdata = state)
  pred <- rnorm(length(pred), pred, sd(residuals(mle)))
  return(pred)
}

set.seed(12345)
res.conf <- boot(state, statistic = conf.fun, R = 1000, mle = mle, ran.gen = rng, sim = "parametric")
e.conf <- envelope(res.conf, level = 0.95)
res.pred <- boot(state, statistic = pred.fun, R = 1000, mle = mle, ran.gen = rng, sim = "parametric")
e.pred <- envelope(res.pred, level = 0.95)


df <- data.frame(MET = state$MET, EX = state$EX, pred = pred.min, 
                 conf_lower_bound = e.conf$point[2,], conf_upper_bound = e.conf$point[1,], 
                 pred_lower_bound = e.pred$point[2,], pred_upper_bound = e.pred$point[1,])

ggplot(df, aes(x = MET)) +
  geom_point(aes(y = EX), fill = "gold1", colour = "black", shape = 21, size = 2) +
  geom_line(aes(y = pred), colour = "grey", linetype = "dashed") + 
  geom_line(aes(y = conf_lower_bound), colour = "blue", alpha = 0.5) +
  geom_line(aes(y = conf_upper_bound), colour = "blue", alpha = 0.5) +
  geom_line(aes(y = pred_lower_bound), colour = "green", alpha = 0.7) +
  geom_line(aes(y = pred_upper_bound), colour = "green", alpha = 0.7) +
  theme_bw() +
  labs(title = "95% confidence bands and prediction bands") +
  theme(plot.margin = margin(.5, .7, .5, .3, "cm"), # graph margin
        axis.title = element_text(size = rel(1.3), face = "bold"), # axis names size
        plot.title = element_text(size = rel(1.6), face = "bold",
                                  hjust = 0.5, margin = unit(c(1, 0, 4, 0), "mm")))