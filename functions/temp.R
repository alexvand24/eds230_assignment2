#' Temperature function
#'
#' This function computes extracts average minimum temperature from any month in the climate data given singular values of temperature and precipitation 
#’ This function is to prepare climate data for a model built by Lobell et al. (2006). 
#’
#' @minimum select minimum or maximum average monthly temperature (minimum = TRUE or minimum = FALSE)
#' @param month_number integer from 1 - 12 to indicate the month of interest where January = 1 and December = 12
#' @author Elmera Azadpour, Mia Forsline, Alex Vand
#' @examples temp(month = 2)
#' @return average minimum temperature per month (ºC)


temp = function(minimum, month_number){
  
  # minimum = TRUE for minimum value // FALSE for maximum value
  # month = number (out of 12) of month of interest
  
  if (minimum == TRUE){
    
    temp <- min(clim_data$tmin_c[clim_data$month == month_number])
    
  } else if (minimum == FALSE){
    
    temp <- max(clim_data$tmax_c[clim_data$month == month_number]) 
    
  } else {
    
    print("Error: must select TRUE (1) or FALSE (0) for minimum input")
    
  }
  
  return(temp)
}
