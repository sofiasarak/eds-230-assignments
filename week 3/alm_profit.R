#' Almond Profit Function
#'
#' @returns
#' @export
#'
#' @examples
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
    
    # Extract necessary variables from respective dfs
    tmin_feb <- climate_month$min_temp_feb
    prec_jan <- precip_month$precip_sum_jan
    
    # Apply formula to variables
    yield <- (-0.015 * tmin_feb) - (0.0046 * (tmin_feb)^2) - (0.07 * prec_jan) + (0.0043 * (prec_jan)^2) + 0.28
    
  }
  
  ## Calculating profit
  # Calculate price per ton
  price_ton <- price_lb * 2000
  
  # Calculate revenue
  revenue <- price_ton * yield
  
  # Calculate final profit
  profit <- revenue - cost_acre
  
  return(profit)
}
  

# notes
# profit = revenue - cost
# revenue = yield * price per unit
# cost = fixed + variable cost
# fixed = machinery price + labor
# variable = price of water
# do we want price to be constant --> adjust for inflation?
# almond yield anomaly will be one input, but profit should depend on actual yield, assume a baseline yield?

# price: need it in tons?
# profit: will be in $/acre
# price: 1.99 per pound; 3,980 per ton (times 2000)

# machinery
# labor
# price of water
# in total, operating cost = on average, $3029 per acre