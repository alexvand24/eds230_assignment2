#' Temperature function 
#'
#' This function computes almond yield anomaly 
#’ given singular values of temperature and precipitation 
#’ This function is based on a model built by Lobell et al. (2006). 
#’
#' @param feb_temp_min minimum temperature in February (degrees C)
#' @param jan_precip January precipitation (mm)
#' @param temp_coeff1 coefficient (ton/acre/degree C), default is -0.015
#' @param temp_coeff2 coefficient (ton/acre/degree C), default is -0.0046
#' @param precip_coeff1 coefficient (ton/acre/mm)
#' @param precip_coeff2 coefficient (ton/acre/mm)
#' @param constant constant (ton/acre), default is 0.28
#' @author Elmera Azadpour, Mia Forsline, Alex Vand
#' @examples almond_yield(temp = 25, precip = 10)
#' @return almond_yield_anomaly (ton/acre)



almond_yield_anomaly = function(feb_temp_min,
                                jan_precip,
                                temp_coeff1 = -0.015,
                                temp_coeff2 = -0.0046,
                                precip_coeff1 = -0.07,
                                precip_coeff2 = 0.0043,
                                constant = 0.28){
  
  almond_yield_anomaly <- (temp_coeff1 * feb_temp_min) +
    (temp_coeff1 * feb_temp_min^2) +
    (precip_coeff1 * jan_precip) +
    (precip_coeff2 * jan_precip^2) +
    constant
  
  return(almond_yield_anomaly)
}
