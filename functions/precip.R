#' Precipitation function
#'
#' This function computes extracts average precipitation from any month in the climate data given singular values of temperature and precipitation 
#’ This function is to prepare climate data for a model built by Lobell et al. (2006). 
#’
#' @param month integer from 1 - 12 to indicate the month of interest where January = 1 and December = 12
#' @author Elmera Azadpour, Mia Forsline, Alex Vand
#' @examples precip(month = 2)
#' @return average monthly precipitation (mm)


precip <- function(month_number){
  
  if( (month_number == round(month_number)) == FALSE ) stop('month number must be an integer 1 - 12')
  
  precip <- mean(clim_data$precip[clim_data$month == month_number])
  
  return(precip) 
}
