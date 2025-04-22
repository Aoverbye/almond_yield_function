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

almond_yield_profit <- function(clim_data, profit_ton = 1400, costs_ton = 1000) {
    
    # Call the almond yield model to get predicted yield (in tons per acre)
    yield_tons <- almond_yield_model(clim_data)
    
    # Calculate profit: (yield per acre) * profit per ton - cost per acre
    profits <- yield_tons * profit_ton - costs_ton
    
    return(profits)
}

