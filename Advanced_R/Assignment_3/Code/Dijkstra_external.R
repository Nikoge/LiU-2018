library(igraph)
library(reshape2)
wiki_graph <- data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6), 
                         v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5), 
                         w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))

dijkstra <- function(graph, init_node) {
z <- acast(graph, graph[,1] ~ graph[,2], value.var = "w")
z[is.na(z)] <- 0
pl2 <- graph.adjacency(z, weighted = TRUE)
plot(pl2)

result <- shortest.paths(pl2, algorithm = "dijkstra")
result <- as.data.frame(result)
result <- result[, init_node]
return(result)
}

dijkstra(wiki_graph, 3)
