## Libraries
library(riem)
library(caret)
library(ggplot2)
library(dplyr)
library(lubridate)

##### IN TEST HERE

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
WeatherData_Clean$valid <- as.POSIXct(WeatherData_Clean$valid)

WeatherData_Clean$valid <- strptime(WeatherData_Clean$valid, format = "%Y-%m-%d %H")
WeatherData_Clean$valid <- as.character(WeatherData_Clean$valid)

## tmpf to tmpc (farenheit to celcius)
WeatherData_Clean$tmpc <- (WeatherData_Clean$tmpf-32)*(5/9)
WeatherData_Clean <- WeatherData_Clean[,-2]

## sknt Wind Speed knots to km/h
WeatherData_Clean$skmh <- WeatherData_Clean$sknt*1.85
WeatherData_Clean <- WeatherData_Clean[,-5]

## dwpf to dwpc
WeatherData_Clean$dwpc <- (WeatherData_Clean$dwpf-32)*(5/9)
WeatherData_Clean <- WeatherData_Clean[,-2]

###############################################

## Import Data
filesList <- list.files("/Users/vascoalbertofiliperibeiro/R/carspeed/RawData")

setwd("/Users/vascoalbertofiliperibeiro/R/carspeed/RawData")

carspeed <- tibble()

for (i in 1:length(filesList)) {
        tmp<- read.csv(filesList[i])   
        
        carspeed <- rbind(tmp, carspeed)
}

## Clean Data

carspeed$Direction <- as.factor(carspeed$Direction)

carspeed_Clean <- filter(carspeed, Speed < 120 & Speed > 10)
carspeed_Clean <- carspeed_Clean[,-2]


carspeed_Clean <- mutate(carspeed_Clean, valid = paste(Date, Time, sep = " "))
carspeed_Clean <- carspeed_Clean[,-c(1,2)]

rm(filesList, carspeed, tmp, i)


carspeed_Clean$valid <- parse_date_time(carspeed_Clean$valid, "Ymd HM", tz="Europe/Lisbon")
carspeed_Clean$valid <- round_date(carspeed_Clean$valid, unit = "hour")
carspeed_filter <- filter(carspeed_Clean, valid >= as.Date("2020-08-03"))

groupedData <- carspeed_filter %>% group_by(valid, Direction)
speed_Analysis <- groupedData %>% summarise(meanSpeed = mean(Speed))

## Change the numeric factor to a more Human readable format

speed_Analysis$Direction <- as.integer(speed_Analysis$Direction)

for(i in 1:length(speed_Analysis$Direction)) {
        if (speed_Analysis$Direction[i] == 1) speed_Analysis$Direction[i] <- "Sintra-MemMartins" 
        else speed_Analysis$Direction[i] <- "MemMartins-Sintra"
        }

speed_Analysis$Direction <- as.factor(speed_Analysis$Direction)

Data <- speed_Analysis$valid
Direção <- speed_Analysis$Direction
Velocidade <- speed_Analysis$meanSpeed

df <- tibble(Data, Direção, Velocidade)

ggplot(data = df, 
       mapping = aes(x = Data, y = Velocidade, color = Direção)) + 
        geom_point(alpha = 1/2) + 
        geom_smooth(method = "lm")

##################################
##### IN TEST HERE

lm_fit <- lm(meanSpeed ~ valid, data=speed_Analysis)
summary(lm_fit)

predicted_df <- data.frame(speed_pred = predict(lm_fit, speed_Analysis), valid = speed_Analysis$valid)

ggplot(data = speed_Analysis, 
       mapping = aes(x = valid, y = meanSpeed, color = Direction)) + 
        geom_point(alpha = 1/2) + 
        geom_line(color='red',data = predicted_df, aes(x=valid, y=speed_pred))


##################################
##### IN TEST HERE

carspeed_LtR <- filter(carspeed_Clean, Direction == 1)
carspeed_RtL <- filter(carspeed_Clean, Direction == 2)


#carspeed_LtR$valid <- parse_date_time(carspeed_LtR$valid, "Ymd HM", tz="Europe/Lisbon")
carspeed_LtR$valid <- as.POSIXct(carspeed_LtR$valid)

#carspeed_RtL$valid <- parse_date_time(carspeed_RtL$valid, "Ymd HM", tz="Europe/Lisbon")
carspeed_RtL$valid <- as.POSIXct(carspeed_RtL$valid)




ggplot(data = carspeed_RtL, mapping = aes(x = valid, y = Speed)) + geom_point(alpha = 1/10)


#carspeed_Clean$valid <- strptime(carspeed_Clean$valid, format = "%Y-%m-%d %H")




#carspeed_Clean$valid <- as.character(carspeed_Clean$valid)

## Merging Data

car_weather <- inner_join(WeatherData_Clean, carspeed_Clean, by = "valid")

## Plot to visualise data