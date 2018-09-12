#**********************************************************************************************************************************#
#*                                                        ASSIGNMENT 3                                                            *#
#*STUDENT NAME: ANUBHAV DIKSHIT                                                                                                   *#
#*LIUID: anudi287                                                                                                                 *#
#**********************************************************************************************************************************#

cat("\014") # Clear console
rm(list = ls()) # clear workspace
gc() #Garbage collector
name <- "Anubhav Dikshit"
liuid <- "anudi287"
setwd("C:/Users/anubh/OneDrive/Documents/GitHub/LiU/LiU-2018/Advanced_R/Assignment_3")

#**********************************************************************************************************************************#
#*                                            LOADING THE LIBRARY IF NOT FOUND INSTALL                                            *#
#**********************************************************************************************************************************#

if(!require(igraph)){install.packages(igraph)} 
if(!require(reshape2)){install.packages(reshape2)} 
library(igraph)
library(reshape2)

#**********************************************************************************************************************************#
#*                                                   ASSIGNMENT 1.1.1 (euclidean)                                                        *#
#**********************************************************************************************************************************#

# 1.1.1 euclidean()

euclidean <- function(a,b){
if(a == 0 | b == 0 | is.na(a) | is.na(b) | !is.numeric(a) | !is.numeric(b)){stop("incorrect inputs")}
if(a > b){
  temp <- b
  b <- a
  a <- temp
}
    r <- b%%a
    return(ifelse(r, euclidean(a, r), a))
}

#**********************************************************************************************************************************#
#*                                                   ASSIGNMENT 1.1.2 (Dijkstra)                                                        *#
#**********************************************************************************************************************************#

dijkstra_ext <- function(graph, init_node) {
  z <- acast(graph, graph[,1] ~ graph[,2], value.var = "w")
  z[is.na(z)] <- 0
  pl2 <- graph.adjacency(z, weighted = TRUE)
  plot(pl2)
  
  result <- shortest.paths(pl2, algorithm = "dijkstra")
  result <- as.data.frame(result)
  result <- result[, init_node]
  return(result)
}

dijkstra_int <- function(df, row) {
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
  return(temp_wide[row,])
  
}






#**********************************************************************************************************************************#
#*                                                   ASSIGNMENT 1.2 (Package Building)                                                        *#
#**********************************************************************************************************************************#

setwd("C:/Users/anubh/OneDrive/Documents/GitHub/LiU/LiU-2018/Advanced_R/Assignment_3")

# create this function and run the following
library(usethis)
use_git_config(user.name = "Anubhav Dikshit", user.email = "anubhav.dikshit@live.in")

#browse_github_pat()

#edit_r_environ()

#restart if never added the key

#Sys.getenv("GITHUB_PAT")

#edit_r_profile() # add default to library descrption

create_package("~/rAssignmentlab3")

use_package("igraph", "reshape2")

wiki_graph <- data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6), 
                         v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5), 
                         w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
use_data(wiki_graph)

devtools::load_all(".")
usethis::use_readme_md()
usethis::use_test()
covr::report()
