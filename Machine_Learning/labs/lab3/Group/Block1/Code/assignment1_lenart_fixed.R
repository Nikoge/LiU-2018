######################################## ASSIGNMENT 1 ########################################
library(geosphere)
library(ggplot2)
options("jtools-digits" = 2, scipen = 999)

# importing data
stations = read.csv2("stations.csv", sep = ",")
temps50k = read.csv2("temps50k.csv", sep = ",")

# merging data
data = merge(x = stations,
             y = temps50k,
             by = "station_number")

temperatureForecast = function(data, date, longitude, latitude, hLocationDist, hDateDist, hHourDist) {

  # identifying all forecast times 
  forecastDateTime = seq(from = as.POSIXct(date),
                         to = as.POSIXct(as.Date(date) + 1),
                         by = "hour")[seq(from = 5, to = 25, by = 2)]
  
  # merging forecast times together with indices to data
  forecastDateTime = data.frame(forecastDateTime, forecastTimeIndex = c(1:length(forecastDateTime)))
  data = merge(data, forecastDateTime, all = TRUE)
  
  # adding distances between observations and forecast object to data
  # location distance
  data$targetLatitude = latitude
  data$targetLongitude = longitude
  
  data$longitude <- as.numeric(as.character(data$longitude))  
  data$latitude <- as.numeric(as.character(data$latitude))
  data$air_temperature <- as.numeric(as.character(data$air_temperature))
  
  data$locationDist = abs(as.numeric(distHaversine(p1 = data[,c("targetLongitude", "targetLatitude")],
                                        p2 = data[,c("longitude", "latitude")], r=6378137)))
  # date distance
  data$dateDist = as.numeric(abs(difftime(data$forecastDateTime, 
                                          data$date, 
                                          units = c("days"))))
  # hour distance
  time = gsub(":", ".", data$time)
  time = as.numeric(gsub(".00", "", time))
  data$hourDist = abs(lubridate::hour(data$forecastDateTime) - time)
  
  # dropping measurements that are post target
  data = data[
    !(difftime(data$forecastDateTime, paste(data$date, data$time), units = c("hour"))) < 0, ]
  
  # calculating gaussian kernels 
  data$locationKernel = exp(-(data$locationDist/hLocationDist)^2)
  data$dateKernel = exp(-(data$dateDist/hDateDist)^2)
  data$hourKernel = exp(-(data$hourDist/hHourDist)^2)
  
  # predicting temperature
  # summation of kernels
  data$summationNumerator = 
    data$locationKernel * (data$air_temperature) +
    data$dateKernel * (data$air_temperature)  +
    data$hourKernel * (data$air_temperature)
  
  data$summationDenominator = data$locationKernel + data$dateKernel + data$hourKernel
  
  # multiplication of kernels
  data$multiplicationNumerator =
    data$locationKernel * data$dateKernel * data$hourKernel * (data$air_temperature)
  data$multiplicationDenominator = data$locationKernel * data$dateKernel * data$hourKernel
  
  # getting prediction results
  temp_sum = c()
  temp_mult = c()
  for (timeIndex in unique(data$forecastTimeIndex)) {
    subset = data[data$forecastTimeIndex == timeIndex,]
    temp_sum[timeIndex] = 
      sum(subset$summationNumerator) / 
      sum(subset$summationDenominator)
    temp_mult[timeIndex] = 
      sum(subset$multiplicationNumerator) / 
      sum(subset$multiplicationDenominator)
  }
  
  # plotting results
  plotData = data.frame(dateTime = forecastDateTime$forecastDateTime,
                        predictionSum = temp_sum,
                        predictionMult = temp_mult)
  ggplot(data = plotData) +
    geom_point(aes(x = dateTime, y = predictionSum, color = "predictionSum")) +
    geom_line(aes(x = dateTime, y = predictionSum, color = "predictionSum")) +
    geom_point(aes(x = dateTime, y = predictionMult, color = "predictionMult")) +
    geom_line(aes(x = dateTime, y = predictionMult, color = "predictionMult")) +
    labs(title = "Predicted temperature", x = "Time", y = "Temperature") +
    theme_bw()
}


temperatureForecast(data = data,
                    date = "2000-05-08",
                    latitude = 59.9953, 
                    longitude = 17.6935, 
                    hLocationDist = 30000, 
                    hDateDist = 2, 
                    hHourDist = 3)
