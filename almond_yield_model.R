# A function for almond yield anomaly as a response to climate 

#Y = "0.015Tn,2 " 0.0046T2
#n;2



#So we would need to filter the month to equal February

almond_yield_test <- function(clim){
    group_by(year) %>%
        summarise(
            avg_feb_temp = mean(tmin_c[month == 2],na.rm = TRUE),
            total_jan_precip = sum(precip[month == 1],na.rm = TRUE)
        ) %>% 
        mutate(anomaly = -0.015 * avg_feb_temp - 0.0046 * avg_feb_temp^2 -
                   0.07 * total_jan_precip + 0.0043 * total_jan_precip^2 + 0.28)
    
    anomaly_min <- min(function_test2$anomaly, na.rm = TRUE)
    anomaly_max <- max(function_test2$anomaly, na.rm = TRUE)
    anomaly_mean <- mean(function_test2$anomaly, na.rm = TRUE)
    
    return(c(anomaly_min, anomaly_max, anomaly_mean))
}

