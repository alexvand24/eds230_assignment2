#' Almond yield anomaly
#'
#' This function computes almond yield anomaly 
#’ given a dataframe of temperature and precipitation data for multiple years. 
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

almond_yield_anomaly_annual = function(feb_temp_min,
                                jan_precip,
                                temp_coeff1 = -0.015,
                                temp_coeff2 = -0.0046,
                                precip_coeff1 = -0.07,
                                precip_coeff2 = 0.0043,
                                constant = 0.28) {
  
  # read in the data from .txt file
  clim_raw <- read.csv(file = here("data", "clim.txt"), 
                       header = T, 
                       sep = "")
  
  # clean data 
  clim_data <- clim_raw %>% 
    janitor::clean_names() %>% 
    mutate(d = lubridate::as_date(d), #convert d column to Date format rather than a character 
           year = lubridate::year(d))
  
  #create a temperature data frame to summarize monthly temperature minimums by year
  temp <- clim_data %>% 
    group_by(month, year) %>% 
    summarize(tmin_c=mean(tmin_c))
  
  #extract the average minimum temperature for February 
  tmin_feb = (temp %>% 
                filter(month==2))$tmin_c 
  
  #create a precipitation data frame to summarize monthly precipitation total by year 
  precip <- clim_data %>% 
    group_by(month, year) %>% 
    summarize(precip =
                sum(precip))
  
  #extract the average precipitation values for January 
  precip_jan = (precip %>% 
                  filter(month==1))$precip
  
  #calculate almond yield anomaly 
  almond_yield_anomaly =
    (temp_coeff1 * tmin_feb) + 
    (temp_coeff2 * tmin_feb**2) +
    (precip_coeff1 * precip_jan) + 
    (precip_coeff2 * precip_jan**2) + 
    constant
  
  return(almond_yield_anomaly)
}
