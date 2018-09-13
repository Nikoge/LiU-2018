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

if(!require(data.table)){install.packages(data.table)}
if(!require(reshape2)){install.packages(reshape2)}
library(data.table)
library(reshape2)
library(dplyr)

#**********************************************************************************************************************************#
#*                                                   ASSIGNMENT 1.1.1 (euclidean)                                                        *#
#**********************************************************************************************************************************#

# 1.1.1 euclidean()

#' Title euclidean() returns the gcd
#'
#' @param a first parameter
#' @param b second parameter
#'
#' @return the gcd
#' @export
#'
#' @examples euclidean(100,1000)
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

#' Title Dijkstra Manual
#'
#' @param graph the input dataframe which represents a network
#' @param init_node the node for which the distance to other nodes must be calculated
#'
#' @return returns a vector with distances to other nodes
#' @export
#'
#' @examples wiki_graph <-
#' data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
#' v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#' w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
#' dijkstra(wiki_graph, 1)

dijkstra_man = function(graph, init_node) {
  # checking input
  if(!is.data.frame(graph) | !is.numeric(init_node)) {
    stop("input is not correct.")
  }
  # initialization of vectors 'nodesToVisit' and 'distances'
  nodesToVisit = c()
  distances = c()
  for (node in unique(graph$v1)) {
    # every distinct node in the graph will be added to the vector 'notesToVisit'
    nodesToVisit = c(nodesToVisit, node)
    # at the beginning, distance to the init_node will be set to zero and to all other notes to infinite
    if(node != init_node) {
      distances[as.character(node)] = Inf
    } else {
      distances[as.character(node)] = 0
    }
  }
  # distance adjustment
  while(length(nodesToVisit) >= 1) {
    # in a loop, always the node of 'nodesToVisit' will the minimal distance to the init_node will be chosen
    currentNode = as.numeric(names(distances)[distances == min(distances[names(distances) %in% nodesToVisit])])[1]
    # for every neigbor of the currentNode, the distance to the init_note will be checked. If the distance is smaller than the currently saved distance, it will be edited.
    for (neighbor in graph[graph$v1 == currentNode,]$v2) {
      if(unname(distances[names(distances) == currentNode]) +
         graph[graph$v1 == currentNode & graph$v2 == neighbor,]$w < (unname(distances[names(distances) == neighbor]))) {
        distances[names(distances) == neighbor] = distances[names(distances) == currentNode] + graph[graph$v1 == currentNode & graph$v2 == neighbor,]$w
      }
    }
    # every node will be just visited once
    nodesToVisit = nodesToVisit[-which(nodesToVisit == currentNode)]
  }
  return(unname(distances))
}


#**********************************************************************************************************************************#
#*                                                   ASSIGNMENT 1.1.2 (Dijkstra)                                                        *#
#**********************************************************************************************************************************#

#' Title dijkstra_adv is the dataframe method for dijkstra
#'
#' @param df is the input dataframe
#' @param node is the node for which the distances are computed
#'
#' @return returns a vector with distance to all other nodes
#' @export
#' @import dplyr left_join bind_rows
#' @import data.table shift
#'
#' @examples wiki_graph <- data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
#'  v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#'  w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
#'  dijkstra_adv(wiki_graph, 1)

dijkstra_adv <- function(df, node) {
  if(is.data.frame(df) & is.numeric(node)){
    n <- length(unique(df[,1]))
    colnames(df) <- c("S", "D", "W")
    result <- df


    for(i in 1:n-1){
      df2 <-  dplyr::left_join(x = result, y = df, by = c("D" = "S"))
      df2$W <- df2$W.x + df2$W.y
      df2 <- df2[,c("S", "D.y", "W")]
      colnames(df2)[colnames(df2)=="D.y"] <- "D"
      result <- dplyr::bind_rows(df2, df)
      result$W <- ifelse(result$S == result$D, 0, result$W) # fixing the self reference distance as zero
      result <- result[!duplicated(result),]
      rm(df2)
    }

    # sorting
    result <- result[with(result, order(S, D, W)), ]
    result$concat <- paste(result$S, result$D, sep = "")
    result$lag_concat <- data.table::shift(result$concat, n=1L, fill=0, type=c("lag"), give.names=FALSE)
    result$change_flag <- ifelse(result$concat == result$lag_concat, 0, 1)

    result <- result[result$change_flag == 1,]
    result <- result[, c("S", "D", "W")]

    temp_wide <- reshape2::dcast(result, S ~ D, value.var = "W", fill = 0)
    rownames(temp_wide) <- NULL
    return(as.vector(temp_wide[,node+1]))
  }
  else{stop("Input must be a dataframe")}}



#**********************************************************************************************************************************#
#*                                                   ASSIGNMENT 1.2 (Package Building)                                                        *#
#**********************************************************************************************************************************#

setwd("C:/Users/anubh/OneDrive/Documents/GitHub/LiU/LiU-2018/Advanced_R/Assignment_3")

# create this function and run the following
library(usethis)

#browse_github_pat()

#edit_r_environ()

#restart if never added the key

#Sys.getenv("GITHUB_PAT")

#edit_r_profile() # add default to library descrption

create_package("~/rAssignmentlab3")
use_mit_license("Anubhav Dikshit")
usethis::use_roxygen_md()

#
# Package: rAssignmentlab3
# Version: 0.0.0.9000
# Title: Package to return GCD and Djisktra implementation
# Description: This package returns the greatest common divisor for any two numbers using Euclidian algorithm. Also implemented the Djisktra algortihm using both external library as well as manual logic.
# Authors@R: c(person("Anubhav", "Dikshit", , email = "anudi287@student.liu.se", role = c("aut", "cre")),
#              person("Lennart", "Schilling", email = "lensc874@student.liu.se", role = c("aut")),
#              person("Thjis", "Quast", email = "thiqu264@student.liu.se", role = c("aut"))
# )
# License: MIT + file LICENSE
# Encoding: UTF-8
# Depends: R (>= 3.5.0)
# LazyData: true
# ByteCompile: true
# Roxygen: list(markdown = TRUE)
# RoxygenNote: 6.1.0
# Imports:
# reshape2 (>= 1.4.3),
# dplyr (>= 0.7.6),
# dplyr (>= 1.11.4)


use_package("reshape2", type = "Imports")
use_package("dplyr", type = "Imports")
use_package("data.table", type = "Imports")
use_data_raw()
wiki_graph <- data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
                         v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
                         w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
use_data(wiki_graph, overwrite = TRUE)


devtools::load_all(".") # run in other window
usethis::use_readme_rmd()

# dijkstra_int(wiki_graph, 1)
# dijkstra_ext(wiki_graph, 1)

#' #' A graph dataset is present here
#' #'
#' #' A dataset containing the source, destination and weight of each node
#' #'
#' #' @format A data frame with 18 rows and 3 variables:
#' #' \describe{
#' #'   \item{v1}{source node}
#' #'   \item{v2}{destination node}
#' #'   \item{w}{weight or distance}
#' #'   ...
#' #' }
#' "wikigraph"

# euclidean(100, 1000)

usethis::use_test("my-test")

# context("test-my-test.R")
# test_that("package works", {
#   expect_equal(euclidean(100, 1000), 100)
#   expect_equal(euclidean(123612, 13892347912), 4)
#   expect_equal(dijkstra_int(wiki_graph, 1), "0 7 9 20 20 11")
#   expect_equal(dijkstra_ext(wiki_graph, 1), "0 7 9 20 20 11")
# })


covr::report()
