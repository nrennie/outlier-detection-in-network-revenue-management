extrapolation_function <- function(data_matrix, alpha=((1:18)/18)^2){
  d <- data_matrix
  #remove all rows with no observations
  d1 <- d[apply(d[,7:24], 1, function(x) !all(is.na(x))),]
  #check which cases are completely observed i.e. are historic data
  d_obs <- d[which(complete.cases(d1)),]
  if (nrow(d_obs) == nrow(data_matrix)){
    return(extrapolation=data_matrix)
  }
  #run extrapolation on remaining values
  d_ext <- d[which(!complete.cases(d1)),]
  #obtain general forecast based on historic data (completely observed)
  g_mat <- historic_forecast_function(d_obs)
  #for each partially observed run extrap
  extrap <- matrix(NA, nrow=nrow(d_ext), ncol=18)
  for (i in 1:nrow(d_ext)){
    x <- d_ext[i,7:24]
    n_obs <- length(x[!is.na(x)])
    #historic forecast
    hist_g <- which(g_mat$`Dep. Day` == weekdays(d_ext[i,1]))
    g <- g_mat[hist_g, (6+n_obs+1):24]
    #arima forecast
    f <- as.vector(forecast(auto.arima(as.numeric(x[1:(n_obs)])),h=18-(n_obs))$mean)
    #weighted forecast
    fc <- as.numeric((1-(alpha[n_obs]))*g + (alpha[n_obs])*f)
    extrap[i,] <- as.numeric(c(x[1:n_obs], fc))
  }
  colnames(extrap) <- colnames(d_obs[,7:24])
  extrap_mat <- cbind(d_ext[,1:6], extrap)
  #join observed and extrapolated together
  extrapolation <- rbind(d_obs, extrap_mat)
  return(extrapolation=extrapolation)
}













