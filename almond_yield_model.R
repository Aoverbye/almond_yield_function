# A function for almond yield anomaly as a response to climate 

#Y = "0.015Tn,2 " 0.0046T2
#n;2


data <- filter(month = 2)

#So we would need to filter the month to equal february

almond_yield_test <- function(clim){
    group_by(year, month) %>%
        summarise(
            feb_temp = mean(tmin_c[month == 2]),
            jan_precip = sum(precip[month == 1])
        )
    anomoly = -0.015 * feb_temp1 - 0.0046 * feb_temp1^2 - .07 * percep1 + .0034 * percep1^2 +.28
    return(anomoly)
}

almond_yield_test(6.263888889, 0)
