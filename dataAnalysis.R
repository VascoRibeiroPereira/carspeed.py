## Libraries
library(riem)
library(caret)
library(ggplot2)

## Get wetaher data

WeatherData <- riem_measures("LPPT", date_start = "2020-08-01",
                               date_end = "2020-08-31")


# Turn NA into zero values
WeatherData[is.na(WeatherData)]<-0

# Getting near zero variance predictors for WeatherData set
nsv_WeatherData <- nearZeroVar(WeatherData)

# Removing the above predictors from both sets
WeatherData_Clean <- (WeatherData[,-nsv_WeatherData])

# Removing predictors that are not useful for the analysis
WeatherData_Clean <- WeatherData_Clean[,-c(7:11)]


# Transforming variables 
## UTC time to WEST
WeatherData_Clean$valid <- format(WeatherData_Clean$valid, tz="Europe/Lisbon", usetz=TRUE)

## tmpf to tmpc (farenheit to celcius)
WeatherData_Clean$tmpc <- (WeatherData_Clean$tmpf-32)*(5/9)
WeatherData_Clean <- WeatherData_Clean[,-2]

## sknt Wind Speed knots to km/h
WeatherData_Clean$skmh <- WeatherData_Clean$sknt*1.85
WeatherData_Clean <- WeatherData_Clean[,-5]

## dwpf to dwpc
WeatherData_Clean$dwpc <- (WeatherData_Clean$dwpf-32)*(5/9)
WeatherData_Clean <- WeatherData_Clean[,-2]


## Plot to visualise data

ggplot(data = WeatherData_Clean, mapping = aes(x = tmpc, y = relh)) + geom