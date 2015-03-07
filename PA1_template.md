# Reproducible Research: Peer Assessment 1

      
## Loading and preprocessing the data

```r
data <- read.csv("activity.csv")
```


## What is mean total number of steps taken per day?
The ddply function is used to calculate the number of steps per day.


```r
library(plyr)
sumSteps <- ddply(data, "date", summarise, steps = sum(steps, na.rm=TRUE))
hist(sumSteps$steps, breaks=10, col='lightblue', main='Histogram of the number of steps', xlim=c(0,25000), xlab='Number of steps')
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
summary(sumSteps$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10400    9354   12810   21190
```


## What is the average daily activity pattern?
First, the dates need to be converted into days of the week. A column with this information is added to the dataframe.
Then the average per day is calculated using the ddply function. The results are plotted and the maximum is calculated.


```r
data$day <- factor(weekdays(as.Date(data$date)))
data$day <- factor(data$day, levels= c("Sunday", "Monday", 
    "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
avgByDay <- ddply(data, .(day, interval), summarise, steps = mean(steps, na.rm=TRUE))
invisible(avgByDay[order(data$day), ])
plot(avgByDay$steps, type='l', xlab='', ylab='Number of steps', xaxt="n")
axis(1, at=c(0,288,576,864,1152,1440,1728,2016), labels=c(levels(avgByDay$day), ""))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
avgByDay[which.max(avgByDay$steps),]
```

```
##         day interval    steps
## 1547 Friday      850 328.5714
```

## Imputing missing values
The missing values are replaced by the mean of that particular interval.


```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

```r
idx <- is.na(data$steps)
modData <- data
for (i in 1:length(idx)) {
      if (idx[i]) {
            modData$steps[i] <- avgByDay$steps[which(avgByDay$day==data$day[i] & avgByDay$interval==data$interval[i])]
      }
}
```
Now, the same methods as before can be used to make a histogram and calculate the mean and median.


```r
sumStepsMod <- ddply(modData, "date", summarise, steps = sum(steps, na.rm=TRUE))
hist(sumStepsMod$steps, breaks=10, col='lightblue', main='Histogram of the number of steps', xlim=c(0,25000), xlab='Number of steps')
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

```r
summary(sumStepsMod$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8918   11020   10820   12810   21190
```
Filling in the missing values slightly changes the values of the minimum, mean, and median of the number of steps.

## Are there differences in activity patterns between weekdays and weekends?
