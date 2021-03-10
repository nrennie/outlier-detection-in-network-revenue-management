mst_clustering_threshold <- function(corr_matrix, leg_names, connections, corr_threshold=0.5){
  colnames(corr_matrix) <- leg_names 
  rownames(corr_matrix) <- leg_names
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
  return(list(cluster_list=cluster_list, adj_mat_corr=as_adjacency_matrix(new_graph, type="both", sparse=F)))
}

#test on simple data
# leg_names <- c("A-B_blue", "B-C_blue", "C-D_blue", "D-E_blue", "F-B_red", "B-C_red", "C-G_red", "G-H_red")
# corr_matrix <- matrix(c(0.00,0.05,0.23,0.10,0.11,0.07,0.08,0.13, #AB_blue
#                        0.05,0.00,0.95,0.64,0.12,0.14,0.13,0.21, #BC_blue
#                        0.23,0.95,0.00,0.89,0.05,0.23,0.18,0.04, #CD_blue
#                        0.10,0.64,0.89,0.00,0.06,0.06,0.04,0.09, #DE_blue
#                        0.11,0.12,0.05,0.06,0.00,0.86,0.05,0.07, #FB_red
#                        0.07,0.14,0.23,0.06,0.86,0.00,0.83,0.64, #BC_red
#                        0.08,0.13,0.18,0.04,0.05,0.83,0.00,0.92, #CG_red
#                        0.13,0.21,0.04,0.09,0.07,0.64,0.92,0.00), ncol=8, nrow=8, byrow=T)#GH_red
# colnames(corr_matrix) <- leg_names
# rownames(corr_matrix) <- leg_names
# connections <- c("F-B_red--B-C_blue", "B-C_red--C-D_blue")
# m <- mst_clustering_threshold(corr_matrix, leg_names, connections, corr_threshold=0.5)
# m$cluster_list
# m$adj_mat_corr
# 




