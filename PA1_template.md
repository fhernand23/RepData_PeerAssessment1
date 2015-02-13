# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
## read data (set working directory to folder with file activity.zip)
data <- read.csv(unz("activity.zip", "activity.csv"), header = TRUE)
```

## What is mean total number of steps taken per day?

1. Total number of steps taken per day


```r
library(dplyr)    
```

```
## Warning: package 'dplyr' was built under R version 3.1.2
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
# filter NA
f <- filter(data, is.na(steps)==0)
# group data by day
g <- group_by(f, date)
# summarise emissions
s <- summarise(g, total_steps=sum(steps))
```

2. Histogram of total number of steps taken each day


```r
hist(s$total_steps, col = "red", main = "Total steps per day", xlab = "Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

3. Mean and median of the total number of steps taken per day

Mean

```r
mean(s$total_steps)
```

```
## [1] 10766.19
```

Median

```r
median(s$total_steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
library(dplyr)    
# filter NA
f <- filter(data, is.na(steps)==0)
# group data by day
g <- group_by(f, interval)
# average steps
s <- summarise(g, avg_steps=mean(steps))
with(s, plot(interval, avg_steps, ylab = "Average steps", xlab = "Interval", type = "l"))
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

2. 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
filter(s, avg_steps == max(s$avg_steps))$interval
```

```
## [1] 835
```

## Imputing missing values

1. Total number of missing values in the dataset


```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

2. Create a new dataset, an fill NA with mean of 5-interval previous calculated


```r
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.1.2
```

```
## -------------------------------------------------------------------------
## You have loaded plyr after dplyr - this is likely to cause problems.
## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
## library(plyr); library(dplyr)
## -------------------------------------------------------------------------
## 
## Attaching package: 'plyr'
## 
## The following objects are masked from 'package:dplyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
```

```r
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
## fill with mean of interval
data_fill <- ddply(data, ~ interval, transform, steps = impute.mean(steps))
```

3. Generate histogram, mean and median


```r
library(dplyr)    
# filter NA
f <- filter(data_fill, is.na(steps)==0)
# group data by day
g <- group_by(f, date)
# summarise emissions
s <- summarise(g, total_steps=sum(steps))
```


```r
hist(s$total_steps, col = "red", main = "Total steps per day", xlab = "Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 

Mean

```r
mean(s$total_steps)
```

```
## [1] 656737.5
```

Median

```r
median(s$total_steps)
```

```
## [1] 656737.5
```

## Are there differences in activity patterns between weekdays and weekends?
