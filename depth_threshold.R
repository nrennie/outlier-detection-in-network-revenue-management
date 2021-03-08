depth_threshold <- function(data,times=c(),perc=0.01,B=1000){
  if (length(times) == 0){
    times <- 1:(ncol(input_data[[1]])-6)
  }
  #data is a data frame of cumulative bookings/forecasts for each flight
  n <- nrow(data)
  rownames(data) <- 1:n
  #array should be t (30) x n (500) x p (1)
  diffs <- array(t(data), dim=c(ncol(data),nrow(data),1))
  weights <- mfd(diffs, time=times, type="projdepth")$MFDdepthZ
  #print(length(weights))
  #create an empty list to store bootstrap samples
  w <- numeric(length = B)
  diff1 <- as.matrix(diffs[,,1])
  for (i in 1:B){
    #create bootstrap samples
    b <- diff1[,sample(1:ncol(diff1),size=ncol(diff1),replace=TRUE,prob=weights)]
    #calculate smoothing matrix
    s <- matrix(mvrnorm(nrow(data),rep(0,ncol(data)),Sigma=0.05*cov(data)),nrow=ncol(data),ncol=n,byrow=TRUE)
    #smooth samples
    y <- array(b+s,dim=c(ncol(data),nrow(data),1))
    #calculate percentile of each set of weights
    k <- mfd(y, time=times, type="projdepth")$MFDdepthZ
    w[[i]] <- sort(k)[ceiling(perc*B)]
  }
  return(median(w))
}
