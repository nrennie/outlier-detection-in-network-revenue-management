#load packages
library(mrfDepth)
library(MASS)
library(POT)
library(xtable)
library(ggplot2)

#load functions
setwd("C:/Users/rennien/OneDrive - Lancaster University/PhD/Simulation/Project 2 - Overlapping/R/Networks")
source("Functional_Depth_p1_Threshold.R")
source("Functional_Depth_p1_Outlier_Detection_diffs.R")
source("GPD_probs.R")
setwd("C:/Users/rennien/OneDrive - Lancaster University/PhD/Simulation/Project 2 - Overlapping/R/Networks/Simulation")
source("bid_price_simulation.R")
source("arrival_probs.R")
source("est_Gamma_params.R")

#load bid prices
setwd("C:/Users/rennien/OneDrive - Lancaster University/PhD/Simulation/Project 2 - Overlapping/R/Networks/Simulation")
bid_prices_AB <- readRDS("bid_prices_AB.rds")
bid_prices_BC <- readRDS("bid_prices_BC.rds")
bid_prices_CD <- readRDS("bid_prices_CD.rds")
bid_prices_DE <- readRDS("bid_prices_DE.rds")

#regular parameters
#c("AB", "AC", "AD", "AE", "BC", "BD", "BE", "CD", "CE", "DE")
means <- c(180,14,14,14,4,4,180,4,14,32) 
vars <- means
alpha <- est_Gamma_params(means,vars)$alphas
beta <- est_Gamma_params(means,vars)$betas
#arrival times
a1 <- c(5,5,5,5,5,5,5,5,5,5)
b1 <- c(2,2,2,2,2,2,2,2,2,2)
a2 <- c(2,2,2,2,2,2,2,2,2,2)
b2 <- c(2,3,5,7,2,3,5,2,3,2)
#purchase probabilities equal for all OD
p1 <- c(0.30,0.25,0.20,0.15,0.10,0.00,0.00) 
p2 <- c(0.00,0.05,0.10,0.15,0.20,0.25,0.25)
#proportions
phi1 <- c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5)
phi2 <- 1 - phi1
#capacities
capacity_AB <- 200
capacity_BC <- 200
capacity_CD <- 200
capacity_DE <- 200
#fare prices 
fare_prices <- matrix(c(70,126,168,196,70,126,168,70,126,70, 
                        60,108,144,168,60,108,144,60,108,60, 
                        50,90,120,140,50,90,120,50,90,50, 
                        40,72,96,112,40,72,96,40,72,40, 
                        30,54,72,84,30,54,72,30,54,30, 
                        20,36,48,56,20,36,48,20,36,20, 
                        10,18,24,28,10,18,24,10,18,10),ncol=10,nrow=7,byrow=T) 
colnames(fare_prices) <- c("AB", "AC", "AD", "AE", "BC", "BD", "BE", "CD", "CE", "DE")
rownames(fare_prices) <- c("A","O","J","P","R","S","M")

#horizon length
horizon <- 1800

#generate bookings for each leg
params_reg <- list(
  rep(list(list(bid_prices_AB, bid_prices_BC, bid_prices_CD, bid_prices_DE, p1, p2, alpha, beta, phi1, phi2, horizon, a1, b1, a2, b2, fare_prices)),360)
)
#add in outlier parameters
means_outliers <-means*1.5
vars_outliers <- vars/5
alpha_outliers <- est_Gamma_params(means_outliers,vars_outliers)$alphas
beta_outliers <- est_Gamma_params(means_outliers,vars_outliers)$betas
params_outliers <- list(
  rep(list(list(bid_prices_AB, bid_prices_BC, bid_prices_CD, bid_prices_DE, p1, p2, alpha_outliers, beta_outliers, phi1, phi2, horizon, a1, b1, a2, b2, fare_prices)),5)
)

#join lists together
params <- c(params_reg[[1]],params_outliers[[1]])

set.seed(68)
b <- lapply(params,function(x) bid_price_simulation(x[[1]],x[[2]],x[[3]],x[[4]],x[[5]],x[[6]],x[[7]],x[[8]],
                                                    x[[9]],x[[10]],x[[11]],x[[12]],x[[13]],x[[14]],x[[15]],x[[16]]))
Bookings_AB <- matrix(unlist(lapply(b, function(l) l[[1]])), ncol = 18, byrow = TRUE)
Bookings_BC <- matrix(unlist(lapply(b, function(l) l[[2]])), ncol = 18, byrow = TRUE)
Bookings_CD <- matrix(unlist(lapply(b, function(l) l[[3]])), ncol = 18, byrow = TRUE)
Bookings_DE <- matrix(unlist(lapply(b, function(l) l[[4]])), ncol = 18, byrow = TRUE)

#resample rows to move outliers around
set.seed(46)
row_order1 <- sample(1:365, 365)
set.seed(44)
row_order2 <- sample(1:365, 365)
Bookings_AB <- Bookings_AB[row_order1,]
Bookings_BC <- Bookings_BC[row_order2,]
Bookings_CD <- Bookings_CD[row_order2,]
Bookings_DE <- Bookings_DE[row_order2,]
rownames(Bookings_AB) <- as.Date(17897:18261, origin="1970-01-01")
rownames(Bookings_BC) <- as.Date(17897:18261, origin="1970-01-01")
rownames(Bookings_CD) <- as.Date(17897:18261, origin="1970-01-01")
rownames(Bookings_DE) <- as.Date(17897:18261, origin="1970-01-01")

#save data 
setwd("C:/Users/rennien/OneDrive - Lancaster University/GitHub/RShiny")
saveRDS(Bookings_AB, "A-B_blue_data.rds")
saveRDS(Bookings_BC, "B-C_blue_data.rds")
saveRDS(Bookings_CD, "C-D_blue_data.rds")
saveRDS(Bookings_DE, "D-E_blue_data.rds")

#calculate diffs and save
outliers_AB <- suppressWarnings(fnoutlier_p1_diffs(data=Bookings_AB, times=1:18, perc=0.01, B=1000, maxiter=50))
outliers_BC <- suppressWarnings(fnoutlier_p1_diffs(data=Bookings_BC, times=1:18, perc=0.01, B=1000, maxiter=50))
outliers_CD <- suppressWarnings(fnoutlier_p1_diffs(data=Bookings_CD, times=1:18, perc=0.01, B=1000, maxiter=50))
outliers_DE <- suppressWarnings(fnoutlier_p1_diffs(data=Bookings_DE, times=1:18, perc=0.01, B=1000, maxiter=50))
names(outliers_AB) <- as.Date(17897:18261, origin="1970-01-01")
names(outliers_BC) <- as.Date(17897:18261, origin="1970-01-01")
names(outliers_CD) <- as.Date(17897:18261, origin="1970-01-01")
names(outliers_DE) <- as.Date(17897:18261, origin="1970-01-01")
#save diffs
setwd("C:/Users/rennien/OneDrive - Lancaster University/GitHub/RShiny")
saveRDS(outliers_AB, "A-B_blue_diffs.rds")
saveRDS(outliers_BC, "B-C_blue_diffs.rds")
saveRDS(outliers_CD, "C-D_blue_diffs.rds")
saveRDS(outliers_DE, "D-E_blue_diffs.rds")

 






###############################################################################################################
#repeat for red line

#regular parameters
#c("AB", "AC", "AD", "AE", "BC", "BD", "BE", "CD", "CE", "DE")
means <- c(32,14,14,180,4,4,14,4,14,32)
vars <- means
alpha <- est_Gamma_params(means,vars)$alphas
beta <- est_Gamma_params(means,vars)$betas
#arrival times
a1 <- c(5,5,5,5,5,5,5,5,5,5)
b1 <- c(2,2,2,2,2,2,2,2,2,2)
a2 <- c(2,2,2,2,2,2,2,2,2,2)
b2 <- c(2,3,5,7,2,3,5,2,3,2)
#purchase probabilities equal for all OD
p1 <- c(0.30,0.25,0.20,0.15,0.10,0.00,0.00) 
p2 <- c(0.00,0.05,0.10,0.15,0.20,0.25,0.25)
#proportions
phi1 <- c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5)
phi2 <- 1 - phi1
#capacities
capacity_AB <- 200
capacity_BC <- 200
capacity_CD <- 200
capacity_DE <- 200
#fare prices 
fare_prices <- matrix(c(70,126,168,196,70,126,168,70,126,70, 
                        60,108,144,168,60,108,144,60,108,60, 
                        50,90,120,140,50,90,120,50,90,50, 
                        40,72,96,112,40,72,96,40,72,40, 
                        30,54,72,84,30,54,72,30,54,30, 
                        20,36,48,56,20,36,48,20,36,20, 
                        10,18,24,28,10,18,24,10,18,10),ncol=10,nrow=7,byrow=T) 
colnames(fare_prices) <- c("AB", "AC", "AD", "AE", "BC", "BD", "BE", "CD", "CE", "DE")
rownames(fare_prices) <- c("A","O","J","P","R","S","M")

#horizon length
horizon <- 1800

#generate bookings for each leg
params_reg <- list(
  rep(list(list(bid_prices_AB, bid_prices_BC, bid_prices_CD, bid_prices_DE, p1, p2, alpha, beta, phi1, phi2, horizon, a1, b1, a2, b2, fare_prices)),360)
)
#add in outlier parameters
means_outliers <-means*1.5
vars_outliers <- vars/5
alpha_outliers <- est_Gamma_params(means_outliers,vars_outliers)$alphas
beta_outliers <- est_Gamma_params(means_outliers,vars_outliers)$betas
params_outliers <- list(
  rep(list(list(bid_prices_AB, bid_prices_BC, bid_prices_CD, bid_prices_DE, p1, p2, alpha_outliers, beta_outliers, phi1, phi2, horizon, a1, b1, a2, b2, fare_prices)),5)
)

#join lists together
params <- c(params_reg[[1]],params_outliers[[1]])

set.seed(67)
b <- lapply(params,function(x) bid_price_simulation(x[[1]],x[[2]],x[[3]],x[[4]],x[[5]],x[[6]],x[[7]],x[[8]],
                                                    x[[9]],x[[10]],x[[11]],x[[12]],x[[13]],x[[14]],x[[15]],x[[16]]))
Bookings_AB <- matrix(unlist(lapply(b, function(l) l[[1]])), ncol = 18, byrow = TRUE)
Bookings_BC <- matrix(unlist(lapply(b, function(l) l[[2]])), ncol = 18, byrow = TRUE)
Bookings_CD <- matrix(unlist(lapply(b, function(l) l[[3]])), ncol = 18, byrow = TRUE)
Bookings_DE <- matrix(unlist(lapply(b, function(l) l[[4]])), ncol = 18, byrow = TRUE)

#resample rows to move outliers around
set.seed(45)
row_order <- sample(1:365, 365)
Bookings_AB <- Bookings_AB[row_order,]
Bookings_BC <- Bookings_BC[row_order,]
Bookings_CD <- Bookings_CD[row_order,]
Bookings_DE <- Bookings_DE[row_order,]
rownames(Bookings_AB) <- as.Date(17897:18261, origin="1970-01-01")
rownames(Bookings_BC) <- as.Date(17897:18261, origin="1970-01-01")
rownames(Bookings_CD) <- as.Date(17897:18261, origin="1970-01-01")
rownames(Bookings_DE) <- as.Date(17897:18261, origin="1970-01-01")
#save data 
setwd("C:/Users/rennien/OneDrive - Lancaster University/GitHub/RShiny")
saveRDS(Bookings_AB, "F-B_red_data.rds")
saveRDS(Bookings_BC, "B-C_red_data.rds")
saveRDS(Bookings_CD, "C-G_red_data.rds")
saveRDS(Bookings_DE, "G-H_red_data.rds")

#calculate diffs and save
outliers_AB <- suppressWarnings(fnoutlier_p1_diffs(data=Bookings_AB, times=1:18, perc=0.01, B=1000, maxiter=50))
outliers_BC <- suppressWarnings(fnoutlier_p1_diffs(data=Bookings_BC, times=1:18, perc=0.01, B=1000, maxiter=50))
outliers_CD <- suppressWarnings(fnoutlier_p1_diffs(data=Bookings_CD, times=1:18, perc=0.01, B=1000, maxiter=50))
outliers_DE <- suppressWarnings(fnoutlier_p1_diffs(data=Bookings_DE, times=1:18, perc=0.01, B=1000, maxiter=50))
names(outliers_AB) <- as.Date(17897:18261, origin="1970-01-01")
names(outliers_BC) <- as.Date(17897:18261, origin="1970-01-01")
names(outliers_CD) <- as.Date(17897:18261, origin="1970-01-01")
names(outliers_DE) <- as.Date(17897:18261, origin="1970-01-01")
#save diffs
setwd("C:/Users/rennien/OneDrive - Lancaster University/GitHub/RShiny")
saveRDS(outliers_AB, "F-B_red_diffs.rds")
saveRDS(outliers_BC, "B-C_red_diffs.rds")
saveRDS(outliers_CD, "C-G_red_diffs.rds")
saveRDS(outliers_DE, "G-H_red_diffs.rds")
