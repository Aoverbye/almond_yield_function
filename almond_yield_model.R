#' Almond Yield Model Function
#' 
#' @authors Matteo Torres, Amanda Overbye
#' 
#' @description The function groups climate data by year and calculates the average February temperature and total January precipitation for each year. It then computes the minimum, maximum, and mean almond yield anomaly of the years in the data set.
#' 
#' @param clim_data A data frame containing climate data with columns for year, month, tmin_c (minimum temperature in Celsius), and precip (precipitation in mm).
#'  
#' @returns A vector containing the minimum, maximum, and mean of the almond yield anomaly based on the years of the data set
#' .
#' @examples almond_yield_model(clim_data)

almond_yield_model <- function(clim_data){
   almond_year_anomaly <- clim_data %>%
        group_by(year) %>%
        summarise(
            avg_feb_temp = mean(tmin_c[month == 2],na.rm = TRUE),
            total_jan_precip = sum(precip[month == 1],na.rm = TRUE)
        ) %>% 
        mutate(anomaly = -0.015 * avg_feb_temp - 0.0046 * avg_feb_temp^2 -
                   0.07 * total_jan_precip + 0.0043 * total_jan_precip^2 + 0.28)
    
    anomaly_min <- min(almond_year_anomaly$anomaly, na.rm = TRUE)
    anomaly_max <- max(almond_year_anomaly$anomaly, na.rm = TRUE)
    anomaly_mean <- mean(almond_year_anomaly$anomaly, na.rm = TRUE)
  
    almond_profit_ton <- 200
    almond_profit_min <- anomaly_min * almond_profit_ton
    almond_profit_max <- anomaly_max * almond_profit_ton
    almond_profit_mean <- anomaly_mean * almond_profit_ton
    
    return(c(almond_profit_min, almond_profit_max, almond_profit_mean))
}

