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
  geom_smooth() +
  theme_bw() +
  ylab("Oceanic Nino Index (ONI)") +
  xlab("Year") +
  labs(title = "Historic Cold & Warm Episodes",
       subtitle = "Monthly Data, Source: NOAA ESRL") 
    

return(plot)


}

### Predict Precip

library(tidyverse)

predict_precip = function(data, ONI=0) {
  model <- lm(PRCP ~ ONI, data = (data))
  prediction <- model$coefficients[1] + ((model$coefficients[2]) * ONI)
  names(prediction) <- NULL
  
  if(ONI > 3)
  return("Error, ONI can only be between -3 and 3")
  
  if(ONI < -3)
  return("Error, ONI can only be between -3 and 3")
  
  else 
  if(prediction < 0) 
    return(list("Rainfall Prediction"= 0))  
    else
    return(list("Rainfall Prediction" =prediction))
}


### Predict Temp

'%!in%' <- function(x,y)!('%in%'(x,y))

possible_months <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")


predict_temp = function(data, month, ONI=0) {
  model <- lm(TAVG ~ month + ONI, data = (data))
  prediction <- model$coefficients[1] + model$coefficients[month] + (ONI * model$coefficients[13])
  prediction_jan <- model$coefficients[1] + (ONI * model$coefficients[13])
  
  
  names(prediction) <- NULL
  names(prediction_jan) <- NULL

  if(ONI > 3)
    return("Error, ONI can only be between -3 and 3")
  
  if(ONI < -3)
    return("Error, ONI can only be between -3 and 3")
  
  else
  if(month %!in% possible_months)
    return(list("Error, month must be between 1 and 12"))
  
  else 
  if(prediction < 0) 
      return(list("Monthly Average Temperature Prediction"= 0))  
  else
    if(month == '1')
    return(list("Monthly Average Temperature Prediction" =prediction_jan))
  else
    return(list("Monthly Average Temperature Prediction" =prediction))
}


### Predict Costs

enso_costs <- function(ONI, risk =.811, gdp =21.9) {
  damages <- (ONI*.06) + risk * gdp

if(ONI > 3)
  return("Error, ONI can only be between -3 and 3")

if(ONI < -3)
  return("Error, ONI can only be between -3 and 3")

else
  if(ONI <= 0.5)
    return("No Increased Risk of Natural Disaster Costs")
  else
    return(list("Expected One Year Increase in Natural Disaster Costs Attributed to El Nino in Billions of USD" =damages))
}


