#'  Modeling forest growth
#' @param time time since start
#' @param C size of forest
#' @param params - as list with two values, r, K, g
#' @param r early exponential growth rate
#' @param K carrying capacity (in units of carbon)
#' @param g linear growth rate
#' @param thresh canopy closure threshold
#' @return derivative of population with time

dforestgrowth <- function(time, C, params, thresh = 50) {
  
  # model if below canopy closure threshold
  if (thresh < 50){
    dC <- params$r * C

  }else{
  
  # model if at or above canopy closure threshold
  dC <- params$g * (1 - C/params$K)
  }
  
  # return list of derivatives
  return(list(dC))
  }
