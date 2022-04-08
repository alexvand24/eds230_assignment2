#' Almond yield anomaly
#'
#' This function computes almond yield anomaly 
#’ given temperature and precipitation data. 
#’ This function is based on a model built by Lobell et al. (2006). 
#' @param temp minimum temperature in February (degrees C)
#' @param precip anuary precipitation (mm)
#' @param temp_coeff1 coefficient (ton/acre/degree C) default is -0.015
#' @param temp_coeff2 coefficient (ton/acre/degree C) default is -0.0046
#' @param precip_coeff1 coefficient (ton/acre/mm)
#' @param precip_coeff2 coefficient (ton/acre/mm)
#' @param constant constant (ton/acre) default is 0.28
#' @author Elmera Azadpour, Mia Forsline, Alex Vand
#' @examples almond_yield(temp = 25, precip = 10)
#' @return almond_yield_anomaly (ton/acre)



almond_yield_anomaly = function(temp,
                                precip,
                                temp_coeff1 = -0.015,
                                temp_coeff2 = -0.0046,
                                precip_coeff1 = -0.07,
                                precip_coeff2 = 0.0043,
                                constant = 0.28){
  
  almond_yield_anomaly <- (temp_coeff1 * temp) + (temp_coeff1 * temp^2) + (precip_coeff1 * precip) + (precip_coeff2 * precip^2) + constant
  
  return(almond_yield_anomaly)
}