# Reproducible Research: Peer Assessment 1



## Loading and Preprocessing the data

```r
activitydata <- read.csv("activity.csv")
library(dplyr)
```
## Mean total number of steps taken per day
1. Histogram of the total numbers of steps taken per day


```r
activitydatadf <- tbl_df(activitydata)
stepsdate <- select(activitydatadf, steps, date)
by_day <- group_by(stepsdate,date)
daysummary <- summarize(by_day,sum(steps))
cnames <- c("Date","Steps")
colnames(daysummary) <- cnames
hist(daysummary$Steps, main = "Total number of steps per day",xlab = "Summary")
```

![plot of chunk unnamed-chunk-2](Figs/unnamed-chunk-2-1.png) 

2. Mean and median total number of steps taken per day


```r
daysummarynona <- na.omit(daysummary)
daysummary2 <- summarize(daysummarynona,mean(Steps), median(Steps))
print(daysummary2)
```

```
## Source: local data frame [1 x 2]
## 
##   mean(Steps) median(Steps)
## 1    10766.19         10765
```
## Average daily activity pattern

1. Time series plot


```r
activitydatadfnona <- na.omit(activitydatadf)
by_interval <- group_by(activitydatadfnona,interval)
intervalsummary <- summarize(by_interval, mean(steps))
cnames3 <- c("interval","meansteps")
colnames(intervalsummary) <- cnames3

with(intervalsummary,plot(interval,meansteps,type = "l",lty=1,lwd=1,ylab="Steps Mean",xlab="Interval"))
```

![plot of chunk unnamed-chunk-4](Figs/unnamed-chunk-4-1.png) 

2. 5-minute interval that, on average across all the days in the dataset, contains the maximum number of steps


```r
intervalsummary2 <- arrange(intervalsummary, desc(meansteps))
Maxinterval <- intervalsummary2$interval[1]
```

## Imputing missing values

1. Total number of missing values in the dataset


```r
activitydataNA <- is.na(activitydata)
numberNA <- sum(activitydataNA)
```

2. Strategy for filling in all of the missing values in the dataset: rrCOvNA package with impSeq function is used to fill the NA values


```r
library("rrcovNA")

stepsinterval <- activitydata[c("interval","steps")]
activitydatanona <- impSeq(stepsinterval)
```

3. New dataset that is equal to the original dataset but with the missing data filled in


```r
date <- activitydata["date"]  
activitydatanonanew <- cbind(date,activitydatanona)   # activitydatanonanew  data frame has all the NAs replaced
```

4. Histogram of the total number of steps taken each day and Calculated  mean and median of total number of steps taken per day


```r
activitydatadfnonanew <- tbl_df(activitydatanonanew)
stepsdatenona <- select(activitydatadfnonanew, steps, date)
by_daynona <- group_by(stepsdatenona,date)
daysummarynonanew <- summarize(by_daynona,sum(steps))
cnames <- c("Date","Steps")
colnames(daysummarynonanew) <- cnames
par(mfcol = c(1, 2))

hist(daysummarynonanew$Steps,main = "Steps per day with NA removed", xlab = "Summary")
hist(daysummary$Steps, main = "Steps per day with NA",xlab = "Summary")
```

![plot of chunk unnamed-chunk-9](Figs/unnamed-chunk-9-1.png) 

```r
daysummary3 <- summarize(daysummarynonanew,mean(Steps), median(Steps))
print(daysummary3)
```

```
## Source: local data frame [1 x 2]
## 
##   mean(Steps) median(Steps)
## 1    10766.19      10766.19
```

```r
### With thre startegy adopted using package rrcovNA, the mean did not change, however the median changed slightly and is same as mean now
```
## Differences in activity patterns between weekdays and weekends

1. New factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day


```r
activitydatanonanew$date <- as.character(activitydatanonanew$date)
activitydatanonanew$date <- as.Date(activitydatanonanew$date)

weekdays <- (weekdays(activitydatanonanew$date))

activitydatanonanew$weekdays <- as.factor(weekdays)

levels(activitydatanonanew$weekdays) <- c("weekday", "weekday", "weekday", "weekday", 
                                    "weekday", "weekend", "weekend")
```

2. Panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)


```r
which_weekdays <- activitydatanonanew$weekdays == "weekday"  ## subset weekday rows
which_weekends <- activitydatanonanew$weekdays == "weekend"  ## subset weekend rows

StepsPerInterval_weekday <- tapply(activitydatanonanew$steps[which_weekdays], as.factor(activitydata$interval[which_weekdays]), 
                                   mean, na.rm = T)
StepsPerInterval_weekend <- tapply(activitydatanonanew$steps[which_weekends], as.factor(activitydata$interval[which_weekends]), 
                                   mean, na.rm = T)

## Plot results in panel plot
par(mfcol = c(2, 1))

plot(levels(as.factor(activitydatanonanew$interval)), StepsPerInterval_weekday, type = "l", 
     xlab = "5 Min interval (hhmm)", ylab = "Average number of steps", main = "Average number of steps per interval weekday", 
     ylim = range(0:250), xlim = range(0:2400))

plot(levels(as.factor(activitydatanonanew$interval)), StepsPerInterval_weekend, type = "l", 
     xlab = "5 Min interval (hhmm)", ylab = "Average number of steps", main = "Average number of steps per interval weekend", 
     ylim = range(0:250), xlim = range(0:2400))
```

![plot of chunk unnamed-chunk-11](Figs/unnamed-chunk-11-1.png) 
## End of assignment
