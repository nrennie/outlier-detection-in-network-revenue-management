#functions
source("processing_function.R")
source("extrapolation_function.R")
source("historic_forecast_function.R")
source("residuals_function.R")
source("correlation_matrix_function.R")
source("mst_clustering_threshold.R")
source("invert_graph.R")
source("depth.R")
source("depth_threshold.R")
source("merge_differences.R")
source("gpd_probs.R")
source("wrapper_function.R")

#packages
source("required_packages.R")

#read in some trial input data in the correct format
setwd("./example_data")
data_A_B_blue <- readRDS("A-B_blue_data.rds")
data_B_C_blue <- readRDS("B-C_blue_data.rds")
data_C_D_blue <- readRDS("C-D_blue_data.rds")
data_D_E_blue <- readRDS("D-E_blue_data.rds")

#input data should be a named list (the structure of the names is very important)
input_data <- list(data_A_B_blue, data_B_C_blue, data_C_D_blue, data_D_E_blue)
names(input_data) <- c("000000A-000000B_blue", "000000B-000000C_blue", "000000C-000000D_blue", "000000D-000000E_blue")

#run outlier detection
wrapper_function(input_data)                            


