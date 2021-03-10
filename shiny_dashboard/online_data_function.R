online_data_function <- function(d, today_date){
  colnames(d) <- c(91,63,42,28, 21, 17, 14, 12, 10, 8, 7, 6, 5, 4, 3, 2, 1,0)
  d1 <- d
  for (i in 1:nrow(d1)){
    for (j in 1:ncol(d1)){
      if (as.Date(as.numeric(rownames(d1)[i]), origin="1970-01-01") - as.numeric(colnames(d1)[j]) > today_date){
        d1[i,j] <- NA
      }
    }
  }
  return(d1)
}

# setwd("C:/Users/rennien/OneDrive - Lancaster University/GitHub/Detecting_Network_Outliers_Online_1day")
# #blue line
# AB_blue_data_20190725 <- online_data_function(readRDS(paste("A-B_blue","_data.rds", sep="")), as.Date("2019-07-25", origin="1970-01-01"))
# saveRDS(AB_blue_data_20190725, "A-B_blue_data_20190725.rds")
# BC_blue_data_20190725 <- online_data_function(readRDS(paste("B-C_blue","_data.rds", sep="")), as.Date("2019-07-25", origin="1970-01-01"))
# saveRDS(BC_blue_data_20190725, "B-C_blue_data_20190725.rds")
# CD_blue_data_20190725 <- online_data_function(readRDS(paste("C-D_blue","_data.rds", sep="")), as.Date("2019-07-25", origin="1970-01-01"))
# saveRDS(CD_blue_data_20190725, "C-D_blue_data_20190725.rds")
# DE_blue_data_20190725 <- online_data_function(readRDS(paste("D-E_blue","_data.rds", sep="")), as.Date("2019-07-25", origin="1970-01-01"))
# saveRDS(DE_blue_data_20190725, "D-E_blue_data_20190725.rds")
# #red line
# FB_red_data_20190725 <- online_data_function(readRDS(paste("F-B_red","_data.rds", sep="")), as.Date("2019-07-25", origin="1970-01-01"))
# saveRDS(FB_red_data_20190725, "F-B_red_data_20190725.rds")
# BC_red_data_20190725 <- online_data_function(readRDS(paste("B-C_red","_data.rds", sep="")), as.Date("2019-07-25", origin="1970-01-01"))
# saveRDS(BC_red_data_20190725, "B-C_red_data_20190725.rds")
# CG_red_data_20190725 <- online_data_function(readRDS(paste("C-G_red","_data.rds", sep="")), as.Date("2019-07-25", origin="1970-01-01"))
# saveRDS(CG_red_data_20190725, "C-G_red_data_20190725.rds")
# GH_red_data_20190725 <- online_data_function(readRDS(paste("G-H_red","_data.rds", sep="")), as.Date("2019-07-25", origin="1970-01-01"))
# saveRDS(GH_red_data_20190725, "G-H_red_data_20190725.rds")



