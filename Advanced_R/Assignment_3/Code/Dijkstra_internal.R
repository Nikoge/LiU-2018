rm(list=ls())
library(dplyr)
library(reshape2)

data <- data.frame(S=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
             D=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
             W=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))

n <- length(unique(data[,1]))
temp <- data

for(i in 1:n-1){
    data2 <-  left_join(x = temp, y = data, by = c("D" = "S"))
    data2$W <- data2$W.x + data2$W.y
    data2 <- data2[,c("S", "D.y", "W")]
    colnames(data2)[colnames(data)=="D.y"] <- "D"
    temp <- rbind(data2, data)
    temp$W <- ifelse(temp$S == temp$D, 0, temp$W) # fixing the self reference distance as zero
    temp <- temp[!duplicated(temp),]
    rm(data2)
}

# sorting
temp <- temp[with(temp, order(S, D, W)), ]
temp$concat <- paste(temp$S, temp$D, sep = "")
temp$lag_concat <- shift(temp$concat, n=1L, fill=0, type=c("lag"), give.names=FALSE)
temp$change_flag <- ifelse(temp$concat == temp$lag_concat, 0, 1)

temp <- temp[temp$change_flag == 1,]
temp <- temp[, c("S", "D", "W")]

temp_wide <- dcast(temp, S ~ D, value.var = "W", fill = NA)
