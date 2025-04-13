# A function for almond yield anomaly as a response to climate 

almond_yield_model <- function(data){
    min = min_daily_temp_feb
    min = min_daily_temp_feb
}

#Y = "0.015Tn,2 " 0.0046T2
#n;2


almond_yield_test <- function(feb_temp1, percep1){
    anomoly = -0.015 * feb_temp1 - 0.0046 * feb_temp1^2 - .07 * percep1 + .0034 * percep1^2 +.28
    return(anomoly)
}

almond_yield_test(6.263888889, 0)
