invert_graph <- function(leg_names, connections){
  #create new nodes from leg names
  n_inv <- length(leg_names)
  adj_mat_inv <- matrix(0, ncol=n_inv, nrow=n_inv)
  rownames(adj_mat_inv) <- leg_names
  colnames(adj_mat_inv) <- leg_names
  #add in edges
  for (i in 1:n_inv){
    for (j in 1:n_inv){
      if (i > j){
        #check if line the same
        i_line <- substr(leg_names[i], start=which(strsplit(leg_names[i], "")[[1]]=="_"), stop=nchar(leg_names[i]))
        j_line <- substr(leg_names[j], start=which(strsplit(leg_names[j], "")[[1]]=="_"), stop=nchar(leg_names[j]))
        i_start <- substr(leg_names[i], start=1, stop=which(strsplit(leg_names[i], "")[[1]]=="-")-1)
        i_stop <- substr(leg_names[i], start=which(strsplit(leg_names[i], "")[[1]]=="-")+1, stop=which(strsplit(leg_names[i], "")[[1]]=="_")-1)
        j_start <- substr(leg_names[j], start=1, stop=which(strsplit(leg_names[j], "")[[1]]=="-")-1)
        j_stop <- substr(leg_names[j], start=which(strsplit(leg_names[j], "")[[1]]=="-")+1, stop=which(strsplit(leg_names[j], "")[[1]]=="_")-1)
        if (i_line == j_line){
          #check if end of i is the start of j
          if (i_stop == j_start | j_stop == i_start){
            adj_mat_inv[i,j] <- 1
            adj_mat_inv[j,i] <- 1
          }
        }
        if (i_line != j_line){
          #else check if connection exists
          if (paste(leg_names[i],leg_names[j], sep = "--", collapse = NULL) %in% connections){
            adj_mat_inv[i,j] <- 1
            adj_mat_inv[j,i] <- 1
          }
          #check if same entry/exit node
          # if (i_stop == j_stop | i_start == j_start){
          #   adj_mat_inv[i,j] <- 1
          #   adj_mat_inv[j,i] <- 1
          # }
        }
      }
    }
  }
  return(adj_mat_inv)
}

# leg_names <- c("A-B_blue", "B-C_blue", "C-D_blue", "D-E_blue", "F-B_red", "B-C_red", "C-G_red", "G-H_red")
# connections <- c("F-B_red--B-C_blue", "B-C_red--C-D_blue")
# g_inv <- invert_graph(leg_names=leg_names, connections=connections)
# g_i=graph.adjacency(adjmatrix=g_inv,mode="undirected")
# plot(g_i)
