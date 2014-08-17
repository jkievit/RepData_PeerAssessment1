# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
knitr::opts_chunk$set(tidy=FALSE, fig.path='figures/')

unzip("activity.zip")
activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date)
```


## What is mean total number of steps taken per day?

Make a histogram of the total number of steps taken each day

```r
steps.per.day <- aggregate(steps~date, data=activity, FUN=sum, na.rm=T)
barplot(steps.per.day$steps, names.arg = steps.per.day$date, xlab = "date", ylab = "total steps")
```

![plot of chunk stepsperday](figures/stepsperday.png) 
  
Calculate and report the **mean** and **median** total number of steps taken per day

```r
mean(steps.per.day$steps)
```

```
## [1] 10766
```

```r
median(steps.per.day$steps)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

Make a time series plot (i.e. ) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
steps.per.interval <- aggregate(steps~interval, data=activity, FUN=mean)
plot(steps.per.interval, type="l")
```

![plot of chunk stepsperinterval](figures/stepsperinterval.png) 
  
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
steps.per.interval$interval[which.max(steps.per.interval$steps)]
```

```
## [1] 835
```
Interval **835** contains the maximum number of steps (**206.1698**)

## Imputing missing values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(activity))
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
  
*The missing values could be replaced by the mean value for that particular interval/weekday combination*

Create a new dataset that is equal to the original dataset but with the missing data filled in.




```r
# add a column with weekday number
activity$weekday <- weekdays(activity$date)

# calulate mean per interval per weekday
steps.pipw <- aggregate(steps~interval+weekday, data=activity, FUN=mean)

# impute missing values using means
act.imputed <- merge(activity, steps.pipw, by=c("interval", "weekday"), suffixes=c("",".mean"))
act.nas <- is.na(act.imputed$steps)
act.imputed$steps[act.nas] <- act.imputed$steps.mean[act.nas]
act.imputed <- act.imputed[,1:4]
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
  

```r
steps.per.day <- aggregate(steps~date, data=act.imputed, FUN=sum)
barplot(steps.per.day$steps, names.arg = steps.per.day$date, xlab = "date", ylab = "total steps")
```

![plot of chunk stepsperdayimputed](figures/stepsperdayimputed.png) 

```r
mean(steps.per.day$steps)
```

```
## [1] 10821
```

```r
median(steps.per.day$steps)
```

```
## [1] 11015
```
  
The mean and median are slightly higher. In this case, the method used for imputing missing values (including weekday as facet) leads to a slightly higher daily estimate. 
  

## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
act.imputed$daytype <- factor(act.imputed$weekday %in% c("Saturday","Sunday"), 
               labels=c("weekday","weekend"), ordered=FALSE)
```

Make a panel plot containing a time series plot (i.e. ) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
par(mfrow = c(2, 1))

agg.weekday <- aggregate(steps~interval, data=act.imputed, subset=act.imputed$daytype=="weekday", FUN=mean)
agg.weekend <- aggregate(steps~interval, data=act.imputed, subset=act.imputed$daytype=="weekend", FUN=mean)
plot(agg.weekend, type="l",  main="weekend")
plot(agg.weekday, type="l", main = "weekday")
```

![plot of chunk daytype](figures/daytype.png) 
  
Weekdays appear to have a higher peak around the start of the day. During the week activity starts earlier in the day.



