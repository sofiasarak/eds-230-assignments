#' alm_yield
#'
#' Compute almond yield based on monthly temperatures and precipitation
#' 
#' @param climate_data a data frame that contains monthly minimum and maximum temperatures (celsius) and precipitation (mm)
#' @returns a vector of years and their corresponding almond yields in tons/acre (`yield`)

alm_yield <- function(climate_data){
  
  # Find mean minimum temperature in February of each year
  climate_month <- climate_data %>% 
    filter(month == 2) %>% 
    group_by(year) %>% 
    summarize(min_temp_feb = mean(tmin_c))
  
  # Find total precipitation in January of each year 
  precip_month <- climate_data %>% 
    filter(month == 1) %>% 
    group_by(year) %>% 
    summarize(precip_sum_jan = sum(precip))
  
  # Extract necessary variables from respective dfs
  tmin_feb <- climate_month$min_temp_feb
  prec_jan <- precip_month$precip_sum_jan
  
  # Apply formula to variables
  yield <- (-0.015 * tmin_feb) - (0.0046 * (tmin_feb)^2) - (0.07 * prec_jan) + (0.0043 * (prec_jan)^2) + 0.28
  
  # Print resulting yield
  print(yield)
}