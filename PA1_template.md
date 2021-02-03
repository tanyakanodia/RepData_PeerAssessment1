---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



```r
setwd("C:/Users/Tanki/RepData_PeerAssessment1")
```
## Loading and preprocessing the data

```r
data <- read.csv("activity.csv")
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
library(ggplot2)
library(Hmisc)
```

```
## Loading required package: lattice
```

```
## Loading required package: survival
```

```
## Loading required package: Formula
```

```
## 
## Attaching package: 'Hmisc'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     src, summarize
```

```
## The following objects are masked from 'package:base':
## 
##     format.pval, units
```


## What is mean total number of steps taken per day?

```r
total_steps <- tapply(data$steps, data$date, sum, na.rm=TRUE)
hist(total_steps, col = "green", 
     breaks = 20,
     main = "Total number of steps taken each day",
     xlab = "Number of steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
mean(total_steps,na.rm = TRUE)
```

```
## [1] 9354.23
```

```r
median(total_steps,na.rm = TRUE)
```

```
## [1] 10395
```


## What is the average daily activity pattern?

```r
by_interval<-group_by(data,interval)
avg<- summarise(by_interval,average=mean(steps,na.rm=TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
ggplot(data=avg, aes(x=interval, y=average)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
##On average across all the days in the dataset, the 5-minute interval contains the maximum number of steps?

```r
avg[which.max(avg$average),1]
```

```
## # A tibble: 1 x 1
##   interval
##      <int>
## 1      835
```

## Imputing missing values

```r
na<-length(which(is.na(data$steps)))
print(paste("Total Na values ", na) )
```

```
## [1] "Total Na values  2304"
```

##Replacing missing values, creating new dataset and creating Histogram

```r
data_filled <- data
data_filled$steps <- impute(data$steps, fun=mean)

total_steps1 <- tapply(data_filled$steps, data_filled$date, sum, na.rm=TRUE)
hist(total_steps, col = "green", 
     breaks = 20,
     main = "Total number of steps taken each day",
     xlab = "Number of steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
mean(total_steps1,na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(total_steps1,na.rm = TRUE)
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?

```r
data_filled$date <- as.Date(data_filled$date)
data_filled$weekday <- weekdays(data_filled$date)
data_filled$day_type<-ifelse(data_filled$weekday %in% c("Sunday","Saturday"), "Weekend","Weekday")

day_types_data <- aggregate(steps ~ interval + day_type, data=data_filled, mean)
ggplot(day_types_data, aes(interval, steps)) + 
        geom_line() + 
        facet_grid(day_type ~ .) +
        xlab("5-minute intervals") + 
        ylab("Avarage number of steps taken") +
        ggtitle("Weekdays and weekends activity patterns")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
