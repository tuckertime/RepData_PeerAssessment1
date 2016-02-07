# Reproducible Research: Peer Assessment 1 
# Stephen Tucker
# Loading and preprocessing the data

# 1. Load the data (i.e. read.csv())

setwd("~/Documents/Rwd/RepData_PeerAssessment1/") # sets working directory 
temp <- read.csv("activity.csv")

# 2. Process/transform the data (if necessary) into a format suitable for your analysis

temp$date <- as.POSIXct(temp$date, format = "%Y-%m-%d")
data <- data.frame(steps = temp$steps,
                  interval = temp$interval, 
                  date = temp$date,
                  day = tolower(weekdays(temp$date)))

# What is the mean total number of steps taken per day?

# 1. Calculate the total number of steps taken per day

totalSteps <- aggregate(data$steps, by = list(date = data$date), sum, na.rm = TRUE)
names(totalSteps) <- c("date","steps")
head(totalSteps)

# 2. Make a histogram of the total number of steps taken each day

hist(totalSteps$steps, 
     breaks = seq(from = 0, to = 22500, by = 500),
     col = "grey",
     xlab = "Total Steps",
     ylab = "Frequency (binwidth 500)",
     main = "Total Number of Steps per Day")
     
# 3. Calculate and report the mean and median total number of steps taken per day

meanSteps <- mean(totalSteps$steps)
meanSteps

medianSteps <- median(totalSteps$steps)
medianSteps

# What is the average daily activity pattern? 

# 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and 
# the average number of steps taken, averaged across all days (y-axis)

pattern <- aggregate(data$steps, by = list(data$interval), mean, na.rm = TRUE)
names(pattern) <- c("interval", "average")
head(pattern)

plot(pattern$interval, pattern$average,
     type = "l",
     col = "gray",
     lwd = 2,
     xlab = "5-Minute Interval",
     ylab = "Average # of Steps",
     main = "Time-series of the average number of steps per 5' interval")

# 2. Which 5-minute interval, on average accross all days in the dataset, contains the 
# maxium number of steps? 

maxSteps <- which.max(pattern$average)
maxSteps

maxInterval <- pattern[maxSteps,1]
maxInterval

# Imputing missing values

# 1. Calculate and report the total number of missing values in the dataset

numNA <- sum(is.na(data$steps))
numNA

# 2. Devise a strategy for filling in all of the missing values in the dataset. 
# The strategy does not need to be sophisticated. For example, you could use the mean/median 
# for that day, or the mean for that 5-minute interval, etc.

# The strategy applied here is to replace missing steps data with the average number of steps. 
# Specifically, a new vector call 'NAvector' is created that identifies positions of missing
# data. An additional vector call AVGvector is created to be the exact length of the NAvector
# and contain all average values. Finally, the NAvector is replaced by the AVGvector. 

NAvector <- which(is.na(data$steps))
AVGvector <- rep(mean(data$steps, na.rm = TRUE), times = length(NAvector))
data[NAvector, "steps"] <- AVGvector
head(data)
                 
# 3. Create a new dataset that is equal to the original dataset but with the missing data 
# filled in.              

data[NAvector, "steps"] <- AVGvector
head(data)

# 4. Make a histogram of the total number of steps taken each day and calculate and report 
# the mean and median total number of steps taken per day. Do these values differ from the 
# estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

totalSteps <- aggregate(data$steps, by = list(date = data$date), sum, na.rm = TRUE)
names(totalSteps) <- c("date","steps")
head(totalSteps)

hist(totalSteps$steps, 
     breaks = seq(from = 0, to = 22500, by = 500),
     col = "grey",
     xlab = "Total Steps",
     ylab = "Frequency (binwidth 500)",
     main = "Total Number of Steps per Day")

meanSteps <- mean(totalSteps$steps)
meanSteps

medianSteps <- median(totalSteps$steps)
medianSteps

# Are there differences in activity patterns between weekdays and weekends?

data <- cbind(data, type = ifelse(data$day == "saturday" | 
                                  data$day == "sunday", "weekend", "weekday"))
head(data)

patterns <- aggregate(steps ~ interval + type, data = data, mean)

library(lattice)

AVGdata <- aggregate(data$steps, by = list(data$type, data$day, data$interval), mean)
names(AVGdata) <- c("type", "day", "interval", "average")
head(AVGdata)

xyplot(average ~ interval | type, AVGdata,
     type = "l",
     col = "grey",
     lwd = 1,
     layout = c(1,2), 
     xlab = "5-Minute Interval",
     ylab = "Average # of Steps",
     main = "Time-series of the Average # of Steps per 5' Interval by Day Type")
     
      




