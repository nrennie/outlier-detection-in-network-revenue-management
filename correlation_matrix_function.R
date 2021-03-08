correlation_matrix_function <- function(input_list, times=c()){
  if (length(times) == 0){
    times <- 1:(ncol(input_data[[1]])-6)
  }
  output <- matrix(NA, ncol=length(input_list), nrow=length(input_list))
  colnames(output) <- names(input_list)
  rownames(output) <- names(input_list)
  for (i in 1:length(input_list)){
    #load ith data
    d1 <- input_list[[i]]
    for (j in 1:length(input_list)){
      #load jth data
      d2 <- input_list[[j]]
      if (i > j) {
        #get common dates 
        common_dates <- intersect(d1$`Dep. Date`, d2$`Dep. Date`)
        fcor <- mean(DynCorr(as.matrix(d1[which(d1$`Dep. Date` %in% common_dates),7:24]), as.matrix(d2[which(d2$`Dep. Date` %in% common_dates),7:24]), t=times))
        output[i,j] <- fcor
        output[j,i] <- fcor
      }
    }
  }
  return(output)
}
