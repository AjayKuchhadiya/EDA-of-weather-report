
# EDA of Weather Report using R environment :

# importing file :
library(readr)
WR <- read_csv("WeatherReport2016.csv")
View(WR)

#Data exploration :
library(tidyverse)

dim(WR)
glimpse(WR) 
head(WR)
tail(WR)
names(WR)
length(WR)

attach(WR)
class(Date.Full)
length(Date.Full)
unique(Date.Full)



# highest and lowest temp of year :

max.temp <- which(Data.Temperature.Max == (max(Data.Temperature.Max)))
min.temp <- which(Data.Temperature.Min == (min(Data.Temperature.Min)))
print(paste('The Hottest day of year is/are : ',Date.Full[max.temp] ))
print(paste('The Coldest day of year is/are : ',Date.Full[min.temp] ))

# summary of data :
summary(WR)

#Drop year column :
WR <- WR[,-5]
View(WR)

#Change date column type from chr to time :
WR$Date.Full <-  as.Date(WR$Date.Full, format =  "%d-%m-%Y")
str(WR)

#Monthly distribution of average temperature :

library("ggplot2")

ggplot(data = WR, mapping = aes(x = Data.Temperature.Avg )) + 
  geom_histogram(fill = 'lightblue', col = 'blue') +
  labs(title = 'Histogram of Monthly Temperature',x = 'Temperature (F)') +
  geom_vline(aes(xintercept = mean(Data.Temperature.Avg,na.rm = TRUE),col = 'mean'), show.legend = TRUE)+
  facet_wrap(~Date.Month)

 
#Polar Bar plot of Wind direction :
ggplot(WR) +
  geom_bar(mapping = aes(x = Data.Wind.Direction),col = 'pink',fill = 'white') +
  coord_polar() +
  labs(title = 'Polar Bar Plot of Wind Direction',x = 'Wind Direction ')


#Correlation Matrix :
de<- WR[-c(2,5,6,7,8,9)]
CM<- cor(de)
View(CM)


library("corrplot") 
corrplot(xx,type = "lower",method = "square",tl.col = "pink",tl.srt = 45)

#To see if there is any relationship of wind speed with temperature :

cor(WR$Data.Wind.Speed, WR$Data.Temperature.Avg) #Negative correlation.

ggplot(data = WR, mapping = aes(x = Data.Temperature.Avg, y = Data.Wind.Speed)) + 
  geom_point(alpha = 0.2)+ stat_smooth(method = "lm") +
  labs(title = 'Scatter Plot of Wind Speed against Average Temperature',
       x = 'Average Temperature ', y = 'Wind Speed')
               # Here we can depict that as the wind speed decreases, the temperature rises.


#Correlation between precipitation and temperature :
ggplot(data = WR, mapping = aes(x = Data.Temperature.Avg, y = Data.Precipitation)) + 
  geom_point(alpha = 0.2)+ stat_smooth(method = "lm") +
  labs(title = 'Scatter Plot of Precipitaion against Average Temperature',
       x = 'Average Temperature ', y = 'Precipitaion')

cor(Data.Temperature.Avg, Data.Precipitation) #Positive correlation.

         # Here we can depict that as the temperature rises, precipitation level increases.
detach(WR)



#The weather report dataset is obtainted from :
# https://corgis-edu.github.io/corgis/csv/weather/

