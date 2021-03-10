dat <- as.matrix(data_matrix[,7:25])
colnames(dat) <- 1:19
rownames(dat) <- c()
fd_d <- fd(t(dat))
month <- factor(months(data_matrix$`Dep. Date`))
day <- factor(weekdays(data_matrix$`Dep. Date`))
func_fit <- fRegress(fd_d ~ month + day)
fitted_curves <- t(as.matrix(func_fit$yhatfdobj$fd$coefs))
#obtain residuals
colnames(fitted_curves) <- 1:19
rownames(fitted_curves) <- c()
res <- dat - fitted_curves
#join residuals to new data frame with info
res_data <- cbind(data_matrix[,1:6], res)
