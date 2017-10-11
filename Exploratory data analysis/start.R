library(geosphere)
library(ggplot2)
library(leaflet)
library(dplyr)
library(lubridate)
library(cluster)   
library(factoextra)


rm(list=ls())
#setwd("D:\\Studies\\Sem 3\\KDD\\Project\\Data")

# Kaggle Dataset 
train_original = read.csv("train.csv")
train = train_original

#load("D:/Studies/Sem 3/KDD/Project/Data/data_train.RData")

# show no missing in train
sum(is.na(train))

# show no missing in test(once we load test data)
#sum(is.na(test))

# code to plot origin
# code to plot destination
# probably make remarks about trips to airports?

# code to show trips vs time of day
train$trip_distance = distHaversine(cbind(train$pickup_longitude, train$pickup_latitude), 
                                    cbind(train$dropoff_longitude, train$dropoff_latitude))/1000
# trip duration in minutes
train$trip_duration = train$trip_duration/60

# duration vs trip
plot(train$trip_distance, train$trip_duration, log = 'xy', 
     xlab = "log distance", ylab = "log duration", main = "Distance vs Duration")


# can kind of see that duration increases with distance

# getting rid of trips longer than 24 hours
train = train[train$trip_duration < 1440, ]

qplot(train$trip_duration, geom="histogram", log = "y", breaks = seq(0, 1500, 60),
      xlab = "Time in minutes", ylab = "log Count", main = "Histogram of Trip Duration")


# let's focus on rides with duration less than a minute!

qplot(train$trip_duration[train$trip_duration < 1], geom="histogram",
      xlab = "Time in minutes", ylab = "Count", main = "Histogram of Trip Duration (Less than a minute)")

# rides with duration of 0 to 20-30sec Dropping them... 
# ride of less than a minute in general would be like taking a cab
# and just getting down.. expection... outlier.. weird behavior..

train = train[train$trip_duration > 1, ]

# focusing on rides with greater than 15 hours
train_10 = train[train$trip_duration > 900, ]

# plotting distance vs duration for this
plot(train_10$trip_distance, train_10$trip_duration, log = 'xy',
     xlab = "log distance", ylab = "log duration", main = "Trips with duration > 15 hours")

rm(train_10)

# can see that max distance 10-100km which should not take 15 hours.. a few outliers 
# with greater distance..
# so again removing them??? idk airport to nyc traffic keep or remove???

train = train[train$trip_duration < 900, ]

train_sample = sample_n(train, 50000)
pickup_location = train_sample[,6:7 ]

# k means clustering to find most common pickup points
pickup = kmeans(pickup_location, centers = 10, iter.max = 100
                , nstart = 10)

# diff approach to plot ..
# gives the lat and long of cluster points.. idk how to use them and plot on map
#pickup$centers
# tried this.. but doesnot show anything
#leaflet(data=pickup_location) %>% addTiles() %>% fitBounds(-74.21, 40.65, -73.17 ,41.02) %>% addCircleMarkers(lng = pickup$centers, lat = pickup$centers, color = "red", opacity = 0.5, radius =1)
# diff approach.. actually plots long and lat on x, y axis...
#plot(pickup_location, col = pickup$cluster, xlim = c(-74.21, -73.6), ylim = c(40.12, 41.02))

# can do the same for dropoff

ansx = as.data.frame(as.table(pickup$centers[, 1]))       
ansy = as.data.frame(as.table(pickup$centers[, 2]))

leaflet(data = pickup_location) %>% addTiles() %>% fitBounds(-74.21, 40.65, -73.17 ,41.02) %>% addCircleMarkers(lng = ansx[, 2], lat = ansy[, 2], color ="blue", opacity = 0.5, radius = 2)
train$trip_duration = train$trip_duration * 60
rm(list=(ls()[ls()!="train"]))


#TIME
train$pickup_datetime = parse_date_time(train$pickup_datetime, orders="ymd HMS")
train$dropoff_datetime = parse_date_time(train$dropoff_datetime, orders="ymd HMS")
train$vendor_id = as.factor(train$vendor_id)
train_original = train

#Plotting the Pickup location
pickup_loc = data.frame(latitude = train$pickup_latitude, longitude = train$pickup_longitude)
pickup_sample = sample_n(pickup_loc, 35000)
leaflet(data=pickup_sample) %>% addTiles() %>% fitBounds(-74.21, 40.65, -73.17 ,41.02) %>% addCircleMarkers(lng = pickup_sample$longitude, lat = pickup_sample$latitude, color = "red", opacity = 0.5, radius =1)

#Plotting the Dropoff location
dropoff_loc = data.frame(latitude = train$dropoff_latitude, longitude = train$dropoff_longitude)
dropoff_sample = sample_n(dropoff_loc, 35000)
leaflet(data=dropoff_sample) %>% addTiles() %>% fitBounds(-74.21, 40.65, -73.17 ,41.02) %>% addCircleMarkers(lng = dropoff_sample$longitude, lat = dropoff_sample$latitude, color = "green", opacity = 0.5, radius = 1)

rm(pickup_loc,pickup_sample,dropoff_sample, dropoff_loc)


# extract date
train$pickup_date = date(train$pickup_datetime)

# extract hour
train$pickup_hour <- hour(train$pickup_datetime)

# find day of the week
train$pickup_day <- weekdays(train$pickup_datetime)
train$pickup_day <- as.factor(train$pickup_day)

# find if weekend
#train$pickup_weekend <- ifelse(train$pickup_weekdays=='Saturday' | train$pickup_weekdays=='Sunday',"Yes","No")
# drop day of the week
train = train[, -14]

# pickup_daytype
train$pickup_daytype <- ifelse(train$pickup_hour >= 7  & train$pickup_hour <= 10, "Morning",
                               ifelse(train$pickup_hour >= 11  & train$pickup_hour <= 15, "Afternoon",
                                      ifelse(train$pickup_hour >= 16  & train$pickup_hour <= 20, "Evening", 
                                             ifelse(train$pickup_hour >= 21  | train$pickup_hour <=03, "Late Night", "Early Morning")
                                             )
                               )
)



train$pickup_daytype <- as.factor(train$pickup_daytype)
summary(train$pickup_daytype)

plot(train$pickup_daytype, col = c("gold","aliceblue","darkorange","gray50", "cadetblue1"), xlab = "Days", ylab = "Count", main = "Distribution of Trips during the Day")
plot(train$vendor_id, col= c("aquamarine", "bisque"), xlab = "Vendor", ylab ="Count", main = "Distribution of Trips by the Vendor" )
plot(train$store_and_fwd_flag, col = c("chocolate", "deepskyblue"), xlab = "Store and Forward", ylab = "Count", main = "Count vs Store and Forward")


