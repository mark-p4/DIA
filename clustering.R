library(igraph)
library(MCL)
library(ggplot2)

clusterMatched = function(matches){
  #convert edges to undirected graph
  adGraph = (graph_from_data_frame(matches[, c("d_id", "a_id")], directed = F))
  
  #use random walk algorithm for clustering
  res = cluster_walktrap(adGraph)
  
  #dataframe for assigning entities to a cluster
  dfClust = data.frame(ids = res$names, clustNo = res$membership) 
  
  
  #assign all matched to a cluster based in the id in a new column
  matches[, "clustNo"] = NA
  for (i in 1:nrow(dfClust)) {
    matches[matches$d_id == dfClust[i, "ids"] | matches$a_id == dfClust[i, "ids"] , "clustNo"] = dfClust[i, "clustNo"]
  }
  
  #based on the degree of an vertex (entity) within a graph a "best choice" is made to which all other entities refer
  matches[, "refersTo"] = NA
  for (i in 1:length(unique(matches$clustNo))) {
    clusterDF = matches[matches$clustNo == i , c("d_id", "a_id")]
    matchGraph = graph_from_data_frame(clusterDF, directed = F)
    maxDegreeVertex = names(which.max(degree(matchGraph)))
    matches[matches$clustNo == i, "refersTo"] = maxDegreeVertex
  }
  return(matches)
}

fullData2 = clusterMatched(fullData1[fullData1$matched, ])
#create a dataframe that contains all edges of matched entities
#matches = similar[,c("d_id", "a_id")]

#matchGraph = graph_from_data_frame(matches[matches$clustNo == 6, c("d_id", "a_id")], directed = F)
#dfmax = which.max(degree(matchGraph)) 
#print(names(dfmax))
#plot(matchGraph)