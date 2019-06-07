#' ONI Functions
#' @title Impacts of ENSO 
#' @description This set of functions finds maximum/minimum of historical ENSO data (1950-), predicts temperature and precipitation for a specific location based on ONI values, and estimates costs based on increased probability of environmental disturbances
#' @param data Data frame with most recent ENSO values 
#' @param ONI Predicted future ONI value
#' @param r Predicted future risk of environmental disturbance
#' @param graph_result if TRUE, output a graphical representation of ENSO values over time
#' @return dependent on function
#'
#' @author Austin Richards and Molly Williams


## Summary functions 

### Find max and min of all data 1950-present

enso_extremes = function(data) {
  
  enso_max <- max(data$ONI)
  enso_min <- min(data$ONI)
  
  return(list("Maximum" = enso_max, "Minimum" = enso_min))

}



### Graph function


enso_plot = function(data) {
  plot <- ggplot(enso_data, aes(x=year, y=ONI)) +
  geom_line() +
  theme_bw() +
  ylab("Oceanic Nino Index (ONI)") +
  xlab("Year") +
  labs(title = "Historic Cold & Warm Episodes",
       subtitle = "Monthly Data, Source: NOAA ESRL") 
    

return(plot)


}



