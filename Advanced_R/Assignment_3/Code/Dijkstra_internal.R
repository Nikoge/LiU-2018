rm(list=ls())
library(dplyr)
library(reshape2)

data <- data.frame(S=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
             D=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
             W=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))

n <- length(unique(data[,1]))
result <- df

for(i in 1:n-1){
    df2 <-  left_join(x = result, y = df, by = c("D" = "S"))
    df2$W <- df2$W.x + df2$W.y
    df2 <- df2[,c("S", "D.y", "W")]
    colnames(df2)[colnames(df)=="D.y"] <- "D"
    result <- rbind(df2, df)
    result$W <- ifelse(result$S == result$D, 0, result$W) # fixing the self reference distance as zero
    result <- result[!duplicated(result),]
    rm(df2)
}

# sorting
result <- result[with(result, order(S, D, W)), ]
result$concat <- paste(result$S, result$D, sep = "")
result$lag_concat <- shift(result$concat, n=1L, fill=0, type=c("lag"), give.names=FALSE)
result$change_flag <- ifelse(result$concat == result$lag_concat, 0, 1)

result <- result[result$change_flag == 1,]
result <- result[, c("S", "D", "W")]

temp_wide <- dcast(result, S ~ D, value.var = "W", fill = NA)
