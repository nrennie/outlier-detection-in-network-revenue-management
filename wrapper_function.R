wrapper_function <- function(input_data, times=c(), perc=0.01, B=1000, maxiter=50, corr_threshold=0.5, connections=c()){
  if (length(times) == 0){
    times <- 1:(ncol(input_data[[1]])-6)
  }
  #prepare data for outlier detection
  extrap_list <- lapply(input_data, function(x) extrapolation_function(x))
  residuals_list <- lapply(extrap_list, function(x) residuals_function(x))
  #clustering procedure
  corr_matrix <- correlation_matrix_function(input_list = residuals_list)
  clustering <- mst_clustering_threshold(corr_matrix=corr_matrix, connections=connections, corr_threshold=corr_threshold)
  #run outlier detection
  depths_list <- lapply(residuals_list, function(x) depth(x[,7:24], times=times, perc=perc, B=B, maxiter=maxiter))
  #for loop for each cluster 
  alert_lists <- list()
  for (i in 1:length(clustering)){
    #extract elements in cluster i from input list
    cluster_i <- depths_list[which(names(depths_list) %in% clustering[[i]])]
    #merge the legs in the cluster
    x <- merge_differences(l=cluster_i)
    #obtain the probability table
    alert_lists[[i]] <- gpd_probs(x)
  }
  names(alert_lists) <- unlist(lapply(clustering, function(x) paste(x,collapse=" ", sep=",")))
  return(alert_lists)
}

