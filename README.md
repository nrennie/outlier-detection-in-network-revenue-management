# Data-driven outlier detection for demand management in transportation networks

The R code corresponding to the paper "Data-driven outlier detection for demand management in transportation networks". Previously submitted to arXiv as ["Detecting outlying demand in multi-leg bookings for transportation networks"](https://arxiv.org/abs/2104.04157).

## Prepare the data

### extrapolation_function.r
Forecasts the remaining bookings for those booking patterns which have not yet departed. The historic data forecasts include day of departure as a factor. Calls historic_forecast_function.R.
 
 
### residuals_function.r
Applies a functional regression to calculate the residual booking patterns. At the moment the only factor in the regression is day of departure. You can easily add in factors for month, year of departure etc. 





## Determine the clusters

### correlation_matrix_function.R
Takes as input a list of legs for which the correlations should be calculated. Returns a matrix with containing the functional dynamical correlations.
 
### mst_clustering_threshold.R
Returns a list of clusters where each list item contains a vector of leg names in each cluster. This function calls invert_graph.R.
 
 
 
 

## Find and aggregate the outliers (run for each cluster)
 
### depth.R
This function takes the output of residuals function as input (plus optional agruments). This should be run for each leg.
This function calls depth_threshold.R. The output is a named vector (names correspond to a uniqueID i.e. departure date).

### merge_differences.R
Input is a named list of vectors. Each vector is the output of depth.R and there should be one vector for each leg within the cluster.

### gpd_probs.R
Takes the output of merge_differences.R as input and produces a data.frame of ranked outliers with columns (unique ID, outlier probability, legs within cluster detected in).



 
 

 
 
 
 
 
 
 
