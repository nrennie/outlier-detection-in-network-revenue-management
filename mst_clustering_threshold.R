mst_clustering_threshold <- function(corr_matrix, connections=c(), corr_threshold=0.5){
  leg_names <- colnames(corr_matrix)
  #invert graph
  g_inv <- invert_graph(leg_names=leg_names, connections=connections)
  g_t <- graph.adjacency(adjmatrix=g_inv, mode="undirected")
  #add weights 
  g_t_edges <- get.edgelist(g_t)
  num_edges <- nrow(g_t_edges)
  g_t_weights <- numeric(length=num_edges)
  for (i in 1:num_edges){
    g_t_weights[i] <- 1 - corr_matrix[g_t_edges[i,1],g_t_edges[i,2]]
  }
  g_t_edges_w <- cbind(g_t_edges, g_t_weights)
  g_t_weighted <- graph.data.frame(g_t_edges_w,directed=FALSE)
  #obtain minimum spanning tree
  mst_g_t <- mst(g_t_weighted)
  #obtain clusters
  mst_edges <- get.edgelist(mst_g_t)
  mst_weights <- get.edge.attribute(mst_g_t)$g_t_weights
  edges_remove <- which(mst_weights > (1 - corr_threshold))
  new_graph <- delete_edges(mst_g_t, edges_remove)
  #return clusters
  components <- decompose(new_graph, min.vertices=1)
  cluster_list <- list()
  for (c in 1:length(components)){
    cluster_list[[c]] <- get.vertex.attribute(components[[c]])$name 
  }
  return(cluster_list=cluster_list)
}


