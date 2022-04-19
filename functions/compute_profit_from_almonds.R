#' compute_profit_from_almonds
#' 
#' This function estimates almond yield profit based on almond yield anomaly, year, price, and discount rate. 
#' 
#' @param  almond_yield_anomaly (ton/acre)
#' @param  year (when almonds were grown, in the format YYYY)
#' @param  price almond price ($/ton)
#' @param  discount rate (a value from 0 - 1; default = 0.12)
#' @author Elmera Azadpour, Mia Forsline, Alex Vand
#' @examples compute_profit_from_almonds(almond_yield_anomaly = ? , year = ? , price = ? , discount = 0.12)
#' @return data frame with estimate of profit ?

compute_profit_from_almonds <- function(almond_yield_anomaly,
                                        year,
                                        price,
                                        discount = 0.12) {
  
  # make sure values are reasonable
  if (length(almond_yield_anomaly) < 1)
    return(NA)
  
  # generate a unique identifier or scenario number
  scen = seq(from = 1, to = length(almond_yield_anomaly))
  yearprofit = data.frame(scen = scen, 
                          almond_yield_anomaly = almond_yield_anomaly, 
                          year = year)
  yearprofit$net = yearprofit$almond_yield_anomaly * price
  
  # note how discount is passed through to this function
  # remember to normalize the year to start year e.g the first year
  yearprofit = yearprofit %>% 
    mutate(netpre = compute_npv(value = net, 
                                time = year - year[1], 
                                discount = discount ))
  
  return(yearprofit)
}
