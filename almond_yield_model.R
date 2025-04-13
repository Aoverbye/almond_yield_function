# A function for almond yield anomaly as a response to climate 

#Y = "0.015Tn,2 " 0.0046T2
#n;2



#So we would need to filter the month to equal february

almond_yield_test <- function(clim){
    group_by(year, month) %>%
        summarise(
            avg_feb_temp = mean(tmin_c[month == 2]),
            total_jan_precip = sum(precip[month == 1])
        ) %>% 
        mutate(anomaly = -0.015 * avg_feb_temp - 0.0046 * avg_feb_temp^2 - .07 * total_jan_percep + .0034 * total_jan_percep1^2 +.28)
    return(anomoly)
}

