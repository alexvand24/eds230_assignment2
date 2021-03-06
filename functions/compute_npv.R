#' Compute net present value of almonds 
#' 
#' This function calculates the net present value of almonds in USD given a current value/cost, an amount of time, and a steady discount rate. 
#' 
#' @param value current price of almonds ($)
#' @param time the future time period during which the price/cost value is being calculated for (years)
#' @param discount rate (a value from 0 - 1; default = 0.12)
#' @author Elmera Azadpour, Mia Forsline, Alex Vand
#' @examples compute_npv(value = 10, time = 2, discount = 0.12)
#' @return value in $


compute_npv <- function(value, time, discount = 0.12) {
  result = value / (1 + discount)**time
  return(result)
}

