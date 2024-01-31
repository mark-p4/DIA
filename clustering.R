library(igraph)
library(MCL)
library(ggplot2)

matches = similar[,c("d_id", "a_id", "jacSim")]

matchMatrix = data.matrix(matches)

#adGraph = get.adjacency(graph_from_data_frame(matches, directed = F))
adGraph = (graph_from_data_frame(matches, directed = F))

mclRes = mcl(adGraph, addLoops = F, max.iter = 3)

res = cluster_walktrap(adGraph, weights = matches$jacSim)

dfClust = data.frame(ids = res$names, clustNo = res$membership) 

#res = cluster_optimal(adGraph)

matches[, "clustNo"] = NA

for (i in 1:nrow(dfClust)) {
  matches[matches$d_id == dfClust[i, "ids"] | matches$a_id == dfClust[i, "ids"] , "clustNo"] = dfClust[i, "clustNo"]
}

matches[, "refersTo"] = NA
for (i in 1:length(unique(matches$clustNo))) {
  clusterDF = matches[matches$clustNo == i , c("d_id", "a_id")]
  matchGraph = graph_from_data_frame(clusterDF, directed = F)
  maxDegreeVertex = names(which.max(degree(matchGraph)))
  matches[matches$clustNo == i, "refersTo"] = maxDegreeVertex
}


matchGraph = graph_from_data_frame(matches[matches$clustNo == 6, c("d_id", "a_id")], directed = F)
dfmax = which.max(degree(matchGraph)) 
print(names(dfmax))
plot(matchGraph)


res$membership

#adGraph = graph_from_adjacency_matrix(adjacency)
plot(adGraph, vertex.size = 1, vertex.label = NA)
adGraph

### Generate adjacency matrix of undirected graph
adjacency <- matrix(c(0,1,1,1,0,0,0,0,0,1,0,1,1,1,0,0,0,0,1,1,
                      0,1,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,1,0,0,
                      0,1,1,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,1,1,
                      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
                    byrow=TRUE, nrow=9)
### Plot graph (requires package igraph)
# library(igraph)
gu <- graph.adjacency( adjacency, mode="undirected" )
# plot( gu )
### Run MCL
res = mcl(x = adjacency, addLoops=TRUE, ESM = TRUE)
### Allow clusters of size 1
res = mcl(x = adjacency, addLoops = TRUE, allow1 = TRUE)
plot(graph_from_adjacency_matrix(res$Equilibrium.state.matrix))
### Error: Small inflation coefficient prevents that an
### equilibrium state matrix is reached within 100 iterations
mcl(x = adjacency, addLoops=TRUE, inflation = 1.01, max.iter = 100)
### Generate adjacency matrix of directed graph
dgr <- matrix(0,nrow = 10,ncol = 10)
dgr[2:3,1] <- 1; dgr[3:4,2] <- 1; dgr[5:6,4] <- 1
dgr[6:7,5] <- 1; dgr[8:9,7] <- 1; dgr[10,8:9] <- 1
### Plot graph (requires package igraph)
# library( igraph )
# gd <- graph.adjacency( dgr )
# plot( gd )
### Directed graphs require self-loops!
mcl(x = dgr, addLoops = TRUE)