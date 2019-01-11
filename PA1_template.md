---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## 1 Loading and preprocessing the data
### 1.1 Load the data

```r
unzip("activity.zip")
Activity <- read.csv("activity.csv",header = T, stringsAsFactors = F)
```

### 1.2 Process the data into a format suitable for the analysis

```r
Activity <- transform(Activity, date = as.Date(date))
```

## 2 What is mean total number of steps taken per day?
### 2.1 Calculate the total number of steps taken per day

```r
TotalStepsPerDay <- tapply(Activity$steps, Activity$date, sum, na.rm=T)
```

### 2.2 Make a histogram of the total number of steps taken each day

```r
hist(TotalStepsPerDay,main = "Histogram of the Total Number of Steps Taken Each Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

### 2.3 Calculate the mean and median of the total number of steps taken per day

```r
mean(TotalStepsPerDay)
```

[1] 9354.23

```r
median(TotalStepsPerDay)
```

[1] 10395

## 3 What is the average daily activity pattern?
### 3.1 Time series plot of the interval and the average number of steps taken

```r
MeanStepsPerInterval <- tapply(Activity$steps, Activity$interval, mean, na.rm=T)
Intervals <- as.numeric(row.names(MeanStepsPerInterval))
plot(Intervals,MeanStepsPerInterval,type="l",ylab = "Average steps across all days")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

### 3.2 The interval, on average across all days, with the maximum number of steps:

```r
names(which.max(MeanStepsPerInterval))
```

[1] "835"


## 4 Imputing missing values
### 4.1 The total number of rows with NAs:

```r
IsNA <- is.na(Activity$steps)
sum(IsNA)
```

[1] 2304

### 4.2-4.3 Use the mean for that interval to fill in the missing values

```r
Activity_Filled <- Activity
for (i in 1:length(Intervals)) {
    Logical <- Activity_Filled$interval==Intervals[i] & IsNA
    Activity_Filled$steps[Logical] <- MeanStepsPerInterval[i]
}
```

### 4.4 Compare with Part 2
Make a histogram of the total number of steps taken each day

```r
TotalStepsPerDay2 <- tapply(Activity_Filled$steps, Activity_Filled$date, sum)
hist(TotalStepsPerDay2,
     main = "Histogram of the Total Number of Steps Taken Each Day (Filled)")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

The mean and median total number of steps taken per day

```r
mean(TotalStepsPerDay2)
```

[1] 10766.19

```r
median(TotalStepsPerDay2)
```

[1] 10766.19

```r
(mean(TotalStepsPerDay2) - mean(TotalStepsPerDay))/mean(TotalStepsPerDay)
```

[1] 0.1509434

```r
(median(TotalStepsPerDay2) - median(TotalStepsPerDay))/median(TotalStepsPerDay)
```

[1] 0.03570839

These values differ from the estimates from Part 2. Imputing missing data increases both the mean and median total number of steps taken per day.

## 5 Are there differences in activity patterns between weekdays and weekends?
### 5.1 Make a new factor variable in the data with levels: weekday and weekend

```r
WeekDate <- weekdays(Activity_Filled$date)
IsWeekend <- WeekDate %in% c("Saturday","Sunday")
WeekDate[IsWeekend] <- "weekend"
WeekDate[!IsWeekend] <- "weekday"
WeekLabel <- factor(WeekDate,order = TRUE,levels = c("weekend","weekday"))
Activity_Filled <- data.frame(Activity_Filled,WeekLabel)
```

### 5.2 Panel plot: the interval (x-axis) VS the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
MeanSteps <- Activity_Filled %>%
             group_by(interval,WeekLabel) %>%
             summarise(Mean = mean(steps))

library(lattice)
xyplot(MeanSteps$Mean ~ MeanSteps$interval | MeanSteps$WeekLabel,
       type = "l",
       layout=c(1,2),
       xlab = "Interval",
       ylab = "Number of steps",
       panel = function(x, y, ...){panel.xyplot(x, y, ...)})
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

