correlation_matrix_function <- function(leg_vec, times=c(1,29,50,64,71,75,78,80,82,84,85,86,87,88,89,90,91,92)){
  output <- matrix(NA, ncol=length(leg_vec), nrow=length(leg_vec))
  colnames(output) <- leg_vec
  rownames(output) <- leg_vec
  for (i in 1:length(leg_vec)){
    #load ith data
    d1 <- readRDS(paste(leg_vec[i],"_data.rds", sep=""))
    for (j in 1:length(leg_vec)){
      #load jth data
      d2 <- readRDS(paste(leg_vec[j],"_data.rds", sep=""))
      if (i > j) {
        #get common dates 
        common_dates <- intersect(rownames(d1), rownames(d2))
        fcor <- mean(DynCorr(as.matrix(d1[which(rownames(d1) %in% common_dates),]), as.matrix(d2[which(rownames(d2) %in% common_dates),]), t=times))
        output[i,j] <- fcor
        output[j,i] <- fcor
      }
    }
  }
  return(output)
}