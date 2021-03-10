library(forecast)
#make function to get extrapolation forecasts 
extrapolate_data <- function(d){
  #remove all rows with no observations
  d1 <- d[apply(d, 1, function(x) !all(is.na(x))),]
  #matrix of which are observed and extrapolated
  obs_extrap_mat <- apply(d1,1:2, function(x) c("Observed", "Extrapolated")[as.numeric(is.na(x))+1])
  #check which cases are completely observed
  d_obs <- d[which(complete.cases(d1)),]
  #run extrapolation on remaining values
  d_ext <- d[which(!complete.cases(d1)),]
  #obtain general forecast based on historic data (completely observed)
  g <- colMeans(d_obs)
  #forecast weighting
  # alpha <- c(1,29,50,64,71,75,78,80,82,84,85,86,87,88,89,90,91,92)/92
  # alpha <- (1:19)/19
  alpha <- ((1:19)/19)^2
  #for each partially observed run extrap
  extrap <- t(apply(d_ext, 1, function(x) c(x[1:(length(x[!is.na(x)]))],(1-alpha[length(x[!is.na(x)])])*(g[((length(x[!is.na(x)]))+1):18]+(x[length(x[!is.na(x)])]-g[length(x[!is.na(x)])])) + (alpha[length(x[!is.na(x)])])*(as.vector(forecast(auto.arima(as.numeric(x[1:(length(x[!is.na(x)]))])),h=18-(length(x[!is.na(x)])))$mean)))))
  #join obsevred and extrapolated together
  extrap_mat <- rbind(d_obs, extrap)
  return(list(extrap_mat=extrap_mat, obs_extrap_mat=obs_extrap_mat))
}

setwd("C:/Users/rennien/OneDrive - Lancaster University/Programming/RShiny/Detecting_Network_Outliers_Online_1day")
source("depth.R")
source("depth_threshold.R")
d <- readRDS("A-B_blue_data_20190725.rds")
k <- extrapolate_data(d)
saveRDS(k$extrap_mat, "A-B_blue_extrap_data_20190725.rds")
saveRDS(func_depth_diffs(k$extrap_mat), "A-B_blue_diffs_20190725.rds")
saveRDS(k$obs_extrap_mat, "A-B_blue_which_extrap_20190725.rds")

d <- readRDS("B-C_blue_data_20190725.rds")
k <- extrapolate_data(d)
saveRDS(k$extrap_mat, "B-C_blue_extrap_data_20190725.rds")
saveRDS(func_depth_diffs(k$extrap_mat), "B-C_blue_diffs_20190725.rds")
saveRDS(k$obs_extrap_mat, "B-C_blue_which_extrap_20190725.rds")

d <- readRDS("C-D_blue_data_20190725.rds")
k <- extrapolate_data(d)
saveRDS(k$extrap_mat, "C-D_blue_extrap_data_20190725.rds")
saveRDS(func_depth_diffs(k$extrap_mat), "C-D_blue_diffs_20190725.rds")
saveRDS(k$obs_extrap_mat, "C-D_blue_which_extrap_20190725.rds")

d <- readRDS("D-E_blue_data_20190725.rds")
k <- extrapolate_data(d)
saveRDS(k$extrap_mat, "D-E_blue_extrap_data_20190725.rds")
saveRDS(func_depth_diffs(k$extrap_mat), "D-E_blue_diffs_20190725.rds")
saveRDS(k$obs_extrap_mat, "D-E_blue_which_extrap_20190725.rds")

d <- readRDS("F-B_red_data_20190725.rds")
k <- extrapolate_data(d)
saveRDS(k$extrap_mat, "F-B_red_extrap_data_20190725.rds")
saveRDS(func_depth_diffs(k$extrap_mat), "F-B_red_diffs_20190725.rds")
saveRDS(k$obs_extrap_mat, "F-B_red_which_extrap_20190725.rds")

d <- readRDS("B-C_red_data_20190725.rds")
k <- extrapolate_data(d)
saveRDS(k$extrap_mat, "B-C_red_extrap_data_20190725.rds")
saveRDS(func_depth_diffs(k$extrap_mat), "B-C_red_diffs_20190725.rds")
saveRDS(k$obs_extrap_mat, "B-C_red_which_extrap_20190725.rds")

d <- readRDS("C-G_red_data_20190725.rds")
k <- extrapolate_data(d)
saveRDS(k$extrap_mat, "C-G_red_extrap_data_20190725.rds")
saveRDS(func_depth_diffs(k$extrap_mat), "C-G_red_diffs_20190725.rds")
saveRDS(k$obs_extrap_mat, "C-G_red_which_extrap_20190725.rds")

d <- readRDS("G-H_red_data_20190725.rds")
k <- extrapolate_data(d)
saveRDS(k$extrap_mat, "G-H_red_extrap_data_20190725.rds")
saveRDS(func_depth_diffs(k$extrap_mat), "G-H_red_diffs_20190725.rds")
saveRDS(k$obs_extrap_mat, "G-H_red_which_extrap_20190725.rds")

