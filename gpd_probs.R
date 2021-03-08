gpd_probs <- function(data_matrix){
  #data matrix is matrix of 
  n <- nrow(data_matrix)
  output_matrix <- data.frame(Dep=character(n), Prob=double(n), Legs=character(n))
  output_matrix$Dep <- rownames(data_matrix)
  #count how many legs an outliers in (vector)
  num_legs_outlier <- apply(data_matrix, 1, function(x) sum(x > 0))
  #sum up values
  diffs <- apply(data_matrix, 1, function(x) sum(x*(x>0)))
  #fit gpd
  m <- suppressWarnings(fitgpd(data=diffs, threshold=0, est = "mle"))
  output_matrix$Prob <- round(pgpd(q=diffs, loc=0, scale=m$fitted.values[1], shape=m$fitted.values[2]), 3)
  for (i in 1:nrow(data_matrix)){
    #add legs affected
    legs_affected <- which(data_matrix[i,] > 0)
    if (length(legs_affected) > 0){
      output_matrix$Legs[i] <- paste(colnames(data_matrix)[legs_affected], collapse = ', ')
    }
  }
  output <- filter(output_matrix, Prob > 0)
  final_output <- output[order(-output$Prob),] 
  rownames(final_output) <- c()
  return(final_output)
}
