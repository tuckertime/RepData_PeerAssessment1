# Reproducible Research: Peer Assessment 1
# Stephen Tucker
## Loading and preprocessing the data
##### 1. Load the data (i.e. read.csv())


```r
setwd("~/Documents/Rwd/RepData_PeerAssessment1/") # sets working directory 
temp <- read.csv("activity.csv")
```

##### 2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
temp$date <- as.POSIXct(temp$date, format = "%Y-%m-%d")
data <- data.frame(steps = temp$steps,
                  interval = temp$interval, 
                  date = temp$date,
                  day = tolower(weekdays(temp$date)))
```

## What is the mean total number of steps taken per day?
##### 1. Calculate the total number of steps taken per day


```r
totalSteps <- aggregate(data$steps, by = list(date = data$date), sum, na.rm = TRUE)
names(totalSteps) <- c("date","steps")
head(totalSteps)
```

```
##         date steps
## 1 2012-10-01     0
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

##### 2. Make a histogram of the total number of steps taken each day


```r
hist(totalSteps$steps, 
     breaks = seq(from = 0, to = 22500, by = 500),
     col = "grey",
     xlab = "Total Steps",
     ylab = "Frequency (binwidth 500)",
     main = "Total Number of Steps per Day")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 
     
##### 3. Calculate and report the mean and median total number of steps taken per day


```r
meanSteps <- mean(totalSteps$steps)
meanSteps
```

```
## [1] 9354.23
```

```r
medianSteps <- median(totalSteps$steps)
medianSteps
```

```
## [1] 10395
```

## What is the average daily activity pattern? 
##### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
pattern <- aggregate(data$steps, by = list(data$interval), mean, na.rm = TRUE)
names(pattern) <- c("interval", "average")
head(pattern)
```

```
##   interval   average
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

```r
plot(pattern$interval, pattern$average,
     type = "l",
     col = "gray",
     lwd = 2,
     xlab = "5-Minute Interval",
     ylab = "Average # of Steps",
     main = "Time-series of the average number of steps per 5' interval")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

##### 2. Which 5-minute interval, on average accross all days in the dataset, contains the maxium number of steps? 


```r
maxSteps <- which.max(pattern$average)
maxSteps
```

```
## [1] 104
```

```r
maxInterval <- pattern[maxSteps,1]
maxInterval
```

```
## [1] 835
```

## Imputing missing values
##### 1. Calculate and report the total number of missing values in the dataset


```r
numNA <- sum(is.na(data$steps))
numNA
```

```
## [1] 2304
```

##### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
NAvector <- which(is.na(data$steps))
AVGvector <- rep(mean(data$steps, na.rm = TRUE), times = length(NAvector))
```

##### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.              


```r
# The strategy applied here is to replace missing steps data with the average number of steps. Specifically, a new vector call 'NAvector' is created that identifies positions of missing data. An additional vector call AVGvector is created to be the exact length of the NAvector and contain all average values. Finally, the NAvector is replaced by the AVGvector. 

NAvector <- which(is.na(data$steps))
AVGvector <- rep(mean(data$steps, na.rm = TRUE), times = length(NAvector))
data[NAvector, "steps"] <- AVGvector
head(data)
```

```
##     steps interval       date    day
## 1 37.3826        0 2012-10-01 monday
## 2 37.3826        5 2012-10-01 monday
## 3 37.3826       10 2012-10-01 monday
## 4 37.3826       15 2012-10-01 monday
## 5 37.3826       20 2012-10-01 monday
## 6 37.3826       25 2012-10-01 monday
```

##### 4. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
totalSteps <- aggregate(data$steps, by = list(date = data$date), sum, na.rm = TRUE)
names(totalSteps) <- c("date","steps")
head(totalSteps)
```

```
##         date    steps
## 1 2012-10-01 10766.19
## 2 2012-10-02   126.00
## 3 2012-10-03 11352.00
## 4 2012-10-04 12116.00
## 5 2012-10-05 13294.00
## 6 2012-10-06 15420.00
```

```r
hist(totalSteps$steps, 
     breaks = seq(from = 0, to = 22500, by = 500),
     col = "grey",
     xlab = "Total Steps",
     ylab = "Frequency (binwidth 500)",
     main = "Total Number of Steps per Day")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

```r
meanSteps <- mean(totalSteps$steps)
meanSteps
```

```
## [1] 10766.19
```

```r
medianSteps <- median(totalSteps$steps)
medianSteps
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?
##### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
data <- cbind(data, type = ifelse(data$day == "saturday" | 
                                  data$day == "sunday", "weekend", "weekday"))
head(data)
```

```
##     steps interval       date    day    type
## 1 37.3826        0 2012-10-01 monday weekday
## 2 37.3826        5 2012-10-01 monday weekday
## 3 37.3826       10 2012-10-01 monday weekday
## 4 37.3826       15 2012-10-01 monday weekday
## 5 37.3826       20 2012-10-01 monday weekday
## 6 37.3826       25 2012-10-01 monday weekday
```

##### 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
library(lattice)

patterns <- aggregate(steps ~ interval + type, data = data, mean)

AVGdata <- aggregate(data$steps, by = list(data$type, data$day, data$interval), mean)
names(AVGdata) <- c("type", "day", "interval", "average")
head(AVGdata)
```

```
##      type      day interval  average
## 1 weekday   friday        0 8.307244
## 2 weekday   monday        0 9.418355
## 3 weekend saturday        0 4.672825
## 4 weekend   sunday        0 4.672825
## 5 weekday thursday        0 9.375844
## 6 weekday  tuesday        0 0.000000
```

```r
xyplot(average ~ interval | type, AVGdata,
     type = "l",
     col = "grey",
     lwd = 1,
     layout = c(1,2), 
     xlab = "5-Minute Interval",
     ylab = "Average # of Steps",
     main = "Time-series of the Average # of Steps per 5' Interval by Day Type")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 



