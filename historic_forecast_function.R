historic_forecast_function <- function(d_obs){
  dat <- as.matrix(d_obs[,7:24])
  rownames(dat) <- c()
  #define factors (could also add month etc.)
  day <- factor(weekdays(d_obs$`Dep. Date`))
  #apply a regression to each time point
  fit_matrix <- apply(dat, 2, function(y) {
    mod <- lm(y ~ day)
    mod_fit <- mod$fitted.values
    return(mod_fit)
  })
  f_mat <- cbind(d_obs[,1:6], fit_matrix)
  f_mat$`Dep. Date` <- day
  pred_mat <- f_mat[!duplicated(round(f_mat[,7:24], 10)),]
  return(pred_mat)
}
