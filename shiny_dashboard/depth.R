func_depth_diffs <- function(data, times=c(1,29,50,64,71,75,78,80,82,84,85,86,87,88,89,90,91,92), perc=0.01, B=1000, maxiter=50){
  #if rownames == NULL
  C <- depth_threshold(data, times=times, perc=perc, B=B)
  n <- nrow(data)
  outlier_names <- rownames(data)
  rownames(data) <- 1:n
  #array should be t (18) x n x p (1)
  d <- array(t(data), dim=c(ncol(data),nrow(data),1))
  depths <- mfd(d, time=times, type="projdepth")$MFDdepthZ
  names(depths) <- outlier_names
  #which are below threshold
  depth_diffs <- numeric(length=n)
  names(depth_diffs) <- outlier_names
  for (i in 1:n){
    depth_diffs[i] <- ((C - depths[i])/C)
  }
  return(depth_diffs)
}
