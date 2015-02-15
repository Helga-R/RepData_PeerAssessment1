Reproducible Research - Process data for Peer Assessment 1



This markdown document uses data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The input file is a CSV file and the variables included in this dataset are:
steps: Number of steps taking in a 5-minute interval (missing values are             
      coded as NA)
date: The date on which the measurement was taken in YYYY-MM-DD format
interval: Identifier for the 5-minute interval in which measurement was taken


#### Loading and preprocessing the data

```r
library(ggplot2)

setwd("C:/BenD/Data Science/RWorkingDir")

Activity <- read.table("activity.csv", sep=",", header=TRUE)

Activity$steps <- as.numeric(Activity$steps)
Activity$date <- as.Date(Activity$date)
Activity$interval <- as.factor(Activity$interval)
```
### What is mean total number of steps taken per day?

#### The total number of steps taken per day

```r
StepsPerDay <- tapply(Activity$steps, Activity$date, sum, na.rm=TRUE)
qplot(StepsPerDay, main="Total steps per day", xlab="Steps Per Day", ylab="Frequency", binwidt=400)
```

![plot of chunk Steps](figure/Steps-1.png) 

#### The mean and median of the total steps by day

```r
MeanStepsPerDay <- mean(StepsPerDay)
MeanStepsPerDay
```

```
## [1] 9354.23
```

```r
MedianStepsPerDay <- median(StepsPerDay)
MedianStepsPerDay
```

```
## [1] 10395
```

###What is the average daily activity pattern?

####The average daily activity pattern

```r
AvgStepsPerDay <- aggregate(steps ~ interval, data = Activity, FUN=mean, na.rm=TRUE)
```
####Plot the series



```r
plot(AvgStepsPerDay, type="l")
```

![plot of chunk plot](figure/plot-1.png) 

#### Time of maximum number of steps

```r
TimeMaxStep <- AvgStepsPerDay$interval[which.max(AvgStepsPerDay$steps)]
TimeMaxStep
```

```
## [1] 835
## 288 Levels: 0 5 10 15 20 25 30 35 40 45 50 55 100 105 110 115 120 ... 2355
```


###Imputing missing values

#### Calculate and report the total number of missing values


```r
MissingValues <- sum(is.na(Activity))
MissingValues
```

```
## [1] 2304
```

#### Mean strategy for filling the missing values


```r
ActivityImputValues <- Activity
nas <- is.na(ActivityImputValues$steps)
ActivityImputValues$steps[nas] <- is.numeric(AvgStepsPerDay)
ActivityImputValues$date <- as.factor(ActivityImputValues$date)
```
#### Create a histogram of the new dataset

```r
StepsPerDayImput <- tapply(ActivityImputValues$steps, ActivityImputValues$date, sum)
qplot(StepsPerDay, main="Total steps per day", xlab="Steps Per Day", ylab="Frequency", binwidth=400)
```

![plot of chunk NewDataset](figure/NewDataset-1.png) 

```r
MeanStepsPerDayImput <- mean(StepsPerDayImput)
MeanStepsPerDayImput
```

```
## [1] 9354.23
```

```r
MedianStepsPerDayImput <- median(StepsPerDayImput)
MedianStepsPerDayImput
```

```
## [1] 10395
```


###Are there differences in activity patterns between weekdays and weekends?

##### Factor which holds type of day


```r
TypeofDay <- function(date) {
  if(weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
    "Weekend"
  } else { 
    "weekday"}
}

ActivityImputValues$TypeofDay <- as.factor(sapply(ActivityImputValues$date, TypeofDay))
```

#### Create a panel plot


```r
AvgStepsPerDayImput <- aggregate(steps ~ interval + TypeofDay, data=ActivityImputValues, FUN=mean)

g <- ggplot(AvgStepsPerDayImput, aes(interval, steps)) +
        geom_line() +
        facet_grid(TypeofDay ~ .) +
        xlab("Interval 5 minutes") +
        ylab("Average number of steps")
print(g)
```

```
## geom_path: Each group consist of only one observation. Do you need to adjust the group aesthetic?
## geom_path: Each group consist of only one observation. Do you need to adjust the group aesthetic?
```

![plot of chunk Panelplot](figure/Panelplot-1.png) 
