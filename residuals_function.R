residuals_function <- function(data_matrix){
  dat <- as.matrix(data_matrix[,7:24])
  colnames(dat) <- 1:18
  rownames(dat) <- c()
  #define factors (could also add months)
  day <- factor(weekdays(data_matrix$`Dep. Date`))
  #apply a regression to each time point
  res_matrix <- apply(dat, 2, function(y) {
    mod <- lm(y ~ day)
    res <- mod$residuals
    return(res)
  })
  output <- cbind(data_matrix[,1:6], res_matrix)
  rownames(output) <- output$`Dep. Date`
  return(output)
}


