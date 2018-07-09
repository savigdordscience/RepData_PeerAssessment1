---
title: "Prog-Assignment-1"
author: "Sharon Avigdor"
date: "June 30, 2018"
output: 
  html_document: 
    keep_md: yes
---



## Read The Data from The Activity File & Format The Date



```r
stepsData <- read.csv("activity.csv", header=T)
stepsData$fixedDate <- as.Date(stepsData$date,format="%Y-%m-%d")

# stepsData$date <- NULL 
total_steps_by_date <- aggregate(list(total_steps = stepsData$steps),
                                 by=list(date = stepsData$date),
                                 FUN=sum,
                                 na.rm=TRUE)
hist(total_steps_by_date$total_steps,
     main="total steps for each date histogram",
     xlab="total steps")
```

![](PA1_template_files/figure-html/readData-1.png)<!-- -->

```r
total_steps_mean <- mean(total_steps_by_date$total_steps)
total_steps_median <- median(total_steps_by_date$total_steps)
```

The mean of total steps is 9354.2295082 and the median is 10395

## What is the average daily activity pattern?


```r
nonNARows <- na.omit(stepsData)
meanIntervals <- tapply(nonNARows$steps, nonNARows$interval, mean)
plot(names(meanIntervals), meanIntervals, type="l", main='avg. steps=f(5 minutes intervals)',xlab = "intervals", ylab = "avg. steps per interval")
```

![](PA1_template_files/figure-html/timeseries-1.png)<!-- -->

```r
index <- match(max(meanIntervals),meanIntervals)  
```

On average across all days in the dataset, the 5-minute interval which contains the maximum number of steps is 835 which is 206.1698113

# Imputing missing values

## Calculating the number of missing values in the dataset
The number of missing values in the dataset is 2304  

## Creating a new dataset with no NAs

```r
newStepsData <- stepsData
for (i in 1:nrow(newStepsData))
{
  if(is.na(newStepsData$steps[i]))
  {
    newStepsData$steps[i] <- meanIntervals[toString(newStepsData$interval[i])]
  }
}
```
## Generating a histogram of the total number of steps taken each day

```r
total_steps_by_date <- aggregate(list(total_steps = newStepsData$steps),
                                 by=list(date = newStepsData$date),
                                 FUN=sum,
                                 na.rm=TRUE)

hist(total_steps_by_date$total_steps,
     main="total steps for each date histogram",
     xlab="total steps")
```

![](PA1_template_files/figure-html/new_total_steps_per_day-1.png)<!-- -->

```r
total_steps_mean <- mean(total_steps_by_date$total_steps)
total_steps_median <- median(total_steps_by_date$total_steps)
```

The mean of total steps is 1.0766189\times 10^{4} and the median is 1.0766189\times 10^{4}

These values differ from the estimates from the first part of the assignment since they are now equal to each other unlike in the first part.

## Differences in activity patterns between weekdays and weekends
1. Add a factor that distincts between a weekday and a weekend

```r
newStepsData$dayType =factor(ifelse(((weekdays(as.Date(newStepsData$date)) == "Sunday") | (weekdays(as.Date(newStepsData$date)) == "Saturday")),"weekend","weekday"),labels = c("weekday","weekend"))
```

2. Panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
par(mfrow=c(2,1))
windows(10,5)
total_steps_by_interval <- aggregate(list(avg_steps = newStepsData$steps),
                                     by=list(interval = newStepsData$interval,dayType = newStepsData$dayType),
                                     FUN=mean,
                                     na.rm=TRUE)
weekday_values <- subset(total_steps_by_interval,dayType=="weekday")
weekend_values <- subset(total_steps_by_interval,dayType=="weekend")
plot(weekday_values$interval, weekday_values$avg_steps, type="l", main='weekday',xlab = "interval", ylab = "number of steps")
```

![](PA1_template_files/figure-html/plot_panel-1.png)<!-- -->

```r
plot(weekend_values$interval, weekend_values$avg_steps, type="l", main='weekend',xlab = "interval", ylab = "number of steps")
```

![](PA1_template_files/figure-html/plot_panel-2.png)<!-- -->
