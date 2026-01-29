#' Almond Profit Function
#' 
#' A function that returns almond production profit based on a number of variables
#'
#' @param price_lb the average price per pound that one can sell an almond for
#' @param cost_acre the average cost of growing an acre of almonds
#' @param climate_data a data frame containing precipitation and temperature data 
#'
#' @returns a data frame with year, almond yield, almond profit, price per pound, and cost per acre
#'
#'
alm_profit <- function(price_lb, cost_acre, climate_data){
  
  ## Creating alm_yield function
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
    
    # Combine to keep year -- important for almond profit calculation
    result <- climate_month %>%
      left_join(precip_month, by = "year")
    
    # Apply formula
    result$yield <- (-0.015 * result$min_temp_feb) - (0.0046 * (result$min_temp_feb)^2) - 
      (0.07 * result$precip_sum_jan) + (0.0043 * (result$precip_sum_jan)^2) + 0.28
    
    return(result %>% select(year, yield))
  }
  
  ## Calculate profit
  result <- alm_yield(climate_data)
  
  # Calculate price per ton
  price_ton <- price_lb * 2000
  
  # Calculate revenue and profit
  result$profit <- (price_ton * result$yield) - cost_acre
  result$price_lb <- price_lb
  result$cost_acre <- cost_acre
  
  return(result)
}
