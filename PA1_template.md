# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
setwd("~/Repos/RepData_PeerAssessment1/")
# Create data directory
if(!file.exists("./data/")){
    dir.create("./data/")}
# Extract File
if(!file.exists("./data/data.csv")) {
  unzip("activity.zip", exdir = "./data/") 
  }
# Read the file
data <- read.csv("./data/activity.csv")
data$date <- as.Date(data$date, format = "%Y-%m-%d")
```

## What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day

```r
library(ggplot2)

daily.steps <- aggregate(steps ~ date, data, FUN = sum)

ggplot(daily.steps) + geom_bar(aes(date,steps),stat="identity") + theme_minimal() + 
  geom_hline(yintercept=mean(daily.steps$steps),colour="red") + 
  geom_hline(yintercept=median(daily.steps$steps),colour="blue",linetype = "dashed")  
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

2. Calculate and report the mean and median total number of steps taken per day

```r
daily.steps.mean <- mean(daily.steps$steps)
daily.steps.median <- median(daily.steps$steps)
```
**Mean:** 1.0766189\times 10^{4}  
**Median:** 10765


## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
steps.interval <- aggregate(steps ~ interval, data, FUN = mean)
ggplot(steps.interval) + geom_line(aes(interval,steps)) + theme_minimal()
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
steps.interval$interval[which.max(steps.interval$steps)]
```

```
## [1] 835
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(data))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.



3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


## Are there differences in activity patterns between weekdays and weekends?
