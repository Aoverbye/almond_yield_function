#' Almond Yield Profit Model Function
#' 
#' @authors Matteo Torres, Amanda Overbye
#' 
#' @description
#' This function uses the almond yield model to calculate predicted almond yields (in tons per acre) for each year in the climate dataset.
#' It then estimates profit per acre using economic parameters: profit per ton and cost per ton of almond yield.
#' The function returns the minimum, maximum, and mean estimated profits per acre based on the modeled yields.
#'
#' @param clim_data A data frame containing climate data with columns for year, month, minimum temperature (°C), and precipitation (mm).
#' @param profit_ton Revenue earned per ton of almond yield (default = 1400).
#' @param costs_ton Cost incurred per ton of almond yield (default = 700).
#'
#' @return A vector with estimated minimum, maximum, and mean profits per acre ($).
#' @export
#'
#' @examples
#' almond_yield_profit(clim_data)
almond_yield_profit <- function(clim_data, profit_ton = 1400, costs_ton = 700) {
    
    yield_tons <- almond_yield_model(clim_data)
    
    net_profit_per_ton <- profit_ton - costs_ton
    
    profits <- yield_tons * net_profit_per_ton
    
    return(profits)
}

