---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
## Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

* Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) 

__The variables included in this dataset are:__

* steps: Number of steps taking in a 5-minute interval (missing values    are coded as 𝙽𝙰) </br>
* date: The date on which the measurement was taken in YYYY-MM-DD format   </br>
* interval: Identifier for the 5-minute interval in which measurement     was taken </br>
* The dataset is stored in a comma-separated-value (CSV) file and there   are a total of 17,568 observations in this dataset. 


## Loading and preprocessing the data
The data is read into R.


```r
library("data.table")
initialdata <- data.table::fread(input = "activity.csv")
```



## What is mean total number of steps taken per day?

Any missing values in the data are ignored.


```r
data <- initialdata[!(is.na(initialdata$steps)),]
```

1. The total number of steps taken per day is calculated. 

For grouping the data into steps taken each day and then calculating the total number of steps, the "aggregate" function is used.


```r
total_steps <- aggregate(steps~date, data, sum)
head(total_steps, 10)
```

```
##          date steps
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
```

A histogram of the total number of steps taken each day is plotted.

```r
library(ggplot2)
ggplot(total_steps, aes(x=steps))+
  geom_histogram(fill = "blue", binwidth = 1000)+
  labs(title = "Daily Steps", x="Steps" , y= "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

The mean and median of the total number of steps taken per day are recorded and calculated.

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:data.table':
## 
##     between, first, last
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
summary <- summarise(total_steps, mean_steps = mean(total_steps$steps),
                     median_steps = median(total_steps$steps))
print(summary)
```

```
##   mean_steps median_steps
## 1   10766.19        10765
```
The mean value is __10766.19__ and the median value is __10765__ .

## What is the average daily activity pattern?
1. A time series plot(i.e.type = "1") is made of the 5-minute interval  (x-axis) and the average number of steps taken, averaged across all   days(y-axis). 

```r
steps_interval <- aggregate(steps ~ interval, data,mean )
head(steps_interval)
```

```
##   interval     steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

```r
library(ggplot2)
ggplot(steps_interval, aes(x = interval , y = steps)) + geom_line(color="blue", size=1) + labs(title = "Average Daily Steps", x = "Interval", y = "Avg. Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

2. The 5-minute interval containing the maximum number of steps on average across all the days in the dataset is found out.


```r
steps_interval[grep(max(steps_interval$steps), steps_interval$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values
1. The total number of missing values in the dataset indicates the total number of rows with NAs. The total number of these missing values is calculated and reported.


```r
sum(is.na(initialdata$steps))
```

```
## [1] 2304
```
2. There are 2304 missing values contained within the steps variable, and a stategy is devised for filling in all these missing values.This strategy involves filling in all the missing values with the median values of that day.

3. Hence, a new dataset equivalent to the original dataset is created, but with the missing values filled in.

```r
newdata <- initialdata
for(x in 1:17568){
  if(is.na(newdata[x,1]) == T){
    newdata[x,1] <- steps_interval[steps_interval$interval %in% newdata[x,3],2]
  }
}
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.71698113207547): 1.716981
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.339622641509434): 0.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.132075471698113): 0.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.150943396226415): 0.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.0754716981132075): 0.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.09433962264151): 2.094340
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.528301886792453): 0.528302
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.867924528301887): 0.867925
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.47169811320755): 1.471698
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.30188679245283): 0.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.132075471698113): 0.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.320754716981132): 0.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.679245283018868): 0.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.150943396226415): 0.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.339622641509434): 0.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.11320754716981): 1.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.83018867924528): 1.830189
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.169811320754717): 0.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')

## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.169811320754717): 0.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.377358490566038): 0.377358
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.264150943396226): 0.264151
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.13207547169811): 1.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.132075471698113): 0.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.226415094339623): 0.226415
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.54716981132075): 1.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.943396226415094): 0.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.207547169811321): 0.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.622641509433962): 0.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.62264150943396): 1.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.584905660377358): 0.584906
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.490566037735849): 0.490566
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.0754716981132075): 0.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.18867924528302): 1.188679
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.943396226415094): 0.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.56603773584906): 2.566038
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.339622641509434): 0.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.358490566037736): 0.358491
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 4.11320754716981): 4.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.660377358490566): 0.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.49056603773585): 3.490566
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.830188679245283): 0.830189
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.11320754716981): 3.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.11320754716981): 1.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.56603773584906): 1.566038
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.24528301886792): 2.245283
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.32075471698113): 3.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.9622641509434): 2.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.09433962264151): 2.094340
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 6.05660377358491): 6.056604
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 16.0188679245283): 16.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 18.3396226415094): 18.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 39.4528301886792): 39.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.4905660377358): 44.490566
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 31.4905660377358): 31.490566
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 49.2641509433962): 49.264151
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 53.7735849056604): 53.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 63.4528301886792): 63.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 49.9622641509434): 49.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 47.0754716981132): 47.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 52.1509433962264): 52.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 39.3396226415094): 39.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.0188679245283): 44.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.1698113207547): 44.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 37.3584905660377): 37.358491
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 49.0377358490566): 49.037736
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.811320754717): 43.811321
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.377358490566): 44.377358
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 50.5094339622642): 50.509434
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 54.5094339622642): 54.509434
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 49.9245283018868): 49.924528
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 50.9811320754717): 50.981132
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 55.6792452830189): 55.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.3207547169811): 44.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 52.2641509433962): 52.264151
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 69.5471698113208): 69.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 57.8490566037736): 57.849057
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 56.1509433962264): 56.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 73.377358490566): 73.377358
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 68.2075471698113): 68.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 129.433962264151): 129.433962
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 157.528301886792): 157.528302
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 171.150943396226): 171.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 155.396226415094): 155.396226
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 177.301886792453): 177.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 206.169811320755): 206.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 195.924528301887): 195.924528
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 179.566037735849): 179.566038
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 183.396226415094): 183.396226
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 167.018867924528): 167.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 143.452830188679): 143.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 124.037735849057): 124.037736
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 109.11320754717): 109.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 108.11320754717): 108.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 103.716981132075): 103.716981
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 95.9622641509434): 95.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 66.2075471698113): 66.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 45.2264150943396): 45.226415
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 24.7924528301887): 24.792453
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 38.7547169811321): 38.754717
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 34.9811320754717): 34.981132
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 21.0566037735849): 21.056604
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 40.5660377358491): 40.566038
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 26.9811320754717): 26.981132
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 42.4150943396226): 42.415094
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 52.6603773584906): 52.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 38.9245283018868): 38.924528
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 50.7924528301887): 50.792453
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.2830188679245): 44.283019
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 37.4150943396226): 37.415094
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 34.6981132075472): 34.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 28.3396226415094): 28.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 25.0943396226415): 25.094340
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 31.9433962264151): 31.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 31.3584905660377): 31.358491
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 29.6792452830189): 29.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 21.3207547169811): 21.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 25.5471698113208): 25.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 28.377358490566): 28.377358
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 26.4716981132075): 26.471698
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 33.4339622641509): 33.433962
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 49.9811320754717): 49.981132
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 42.0377358490566): 42.037736
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.6037735849057): 44.603774
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 46.0377358490566): 46.037736
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 59.188679245283): 59.188679
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 63.8679245283019): 63.867925
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 87.6981132075472): 87.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 94.8490566037736): 94.849057
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 92.7735849056604): 92.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 63.3962264150943): 63.396226
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 50.1698113207547): 50.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 54.4716981132075): 54.471698
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 32.4150943396226): 32.415094
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 26.5283018867925): 26.528302
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 37.7358490566038): 37.735849
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 45.0566037735849): 45.056604
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 67.2830188679245): 67.283019
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 42.3396226415094): 42.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 39.8867924528302): 39.886792
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.2641509433962): 43.264151
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 40.9811320754717): 40.981132
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 46.2452830188679): 46.245283
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 56.4339622641509): 56.433962
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 42.7547169811321): 42.754717
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 25.1320754716981): 25.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 39.9622641509434): 39.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 53.5471698113208): 53.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 47.3207547169811): 47.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 60.811320754717): 60.811321
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 55.7547169811321): 55.754717
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 51.9622641509434): 51.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.5849056603774): 43.584906
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 48.6981132075472): 48.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 35.4716981132075): 35.471698
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 37.5471698113208): 37.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 41.8490566037736): 41.849057
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 27.5094339622642): 27.509434
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 17.1132075471698): 17.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 26.0754716981132): 26.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.622641509434): 43.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.7735849056604): 43.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 30.0188679245283): 30.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 36.0754716981132): 36.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 35.4905660377358): 35.490566
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 38.8490566037736): 38.849057
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 45.9622641509434): 45.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 47.7547169811321): 47.754717
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 48.1320754716981): 48.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 65.3207547169811): 65.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 82.9056603773585): 82.905660
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 98.6603773584906): 98.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 102.11320754717): 102.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 83.9622641509434): 83.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 62.1320754716981): 62.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 64.1320754716981): 64.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 74.5471698113208): 74.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 63.1698113207547): 63.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 56.9056603773585): 56.905660
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 59.7735849056604): 59.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.8679245283019): 43.867925
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 38.5660377358491): 38.566038
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.6603773584906): 44.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 45.4528301886792): 45.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 46.2075471698113): 46.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.6792452830189): 43.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 46.622641509434): 46.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 56.3018867924528): 56.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 50.7169811320755): 50.716981
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 61.2264150943396): 61.226415
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 72.7169811320755): 72.716981
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 78.9433962264151): 78.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 68.9433962264151): 68.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 59.6603773584906): 59.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 75.0943396226415): 75.094340
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 56.5094339622642): 56.509434
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 34.7735849056604): 34.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 37.4528301886792): 37.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 40.6792452830189): 40.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 58.0188679245283): 58.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 74.6981132075472): 74.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 85.3207547169811): 85.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 59.2641509433962): 59.264151
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 67.7735849056604): 67.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 77.6981132075472): 77.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 74.2452830188679): 74.245283
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 85.3396226415094): 85.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 99.4528301886792): 99.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 86.5849056603774): 86.584906
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 85.6037735849057): 85.603774
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 84.8679245283019): 84.867925
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 77.8301886792453): 77.830189
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 58.0377358490566): 58.037736
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 53.3584905660377): 53.358491
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 36.3207547169811): 36.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 20.7169811320755): 20.716981
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 27.3962264150943): 27.396226
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 40.0188679245283): 40.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 30.2075471698113): 30.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 25.5471698113208): 25.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 45.6603773584906): 45.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 33.5283018867925): 33.528302
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 19.622641509434): 19.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 19.0188679245283): 19.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 19.3396226415094): 19.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 33.3396226415094): 33.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 26.811320754717): 26.811321
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 21.1698113207547): 21.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 27.3018867924528): 27.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 21.3396226415094): 21.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 19.5471698113208): 19.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 21.3207547169811): 21.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 32.3018867924528): 32.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 20.1509433962264): 20.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 15.9433962264151): 15.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 17.2264150943396): 17.226415
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 23.4528301886792): 23.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 19.2452830188679): 19.245283
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 12.4528301886792): 12.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 8.0188679245283): 8.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 14.6603773584906): 14.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 16.3018867924528): 16.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 8.67924528301887): 8.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 7.79245283018868): 7.792453
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 8.13207547169811): 8.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.62264150943396): 2.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.45283018867925): 1.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.67924528301887): 3.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 4.81132075471698): 4.811321
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 8.50943396226415): 8.509434
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 7.07547169811321): 7.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 8.69811320754717): 8.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 9.75471698113208): 9.754717
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.20754716981132): 2.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.320754716981132): 0.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.113207547169811): 0.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.60377358490566): 1.603774
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 4.60377358490566): 4.603774
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.30188679245283): 3.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.84905660377358): 2.849057
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.830188679245283): 0.830189
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.962264150943396): 0.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.58490566037736): 1.584906
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.60377358490566): 2.603774
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 4.69811320754717): 4.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.30188679245283): 3.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.641509433962264): 0.641509
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.226415094339623): 0.226415
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.07547169811321): 1.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.71698113207547): 1.716981
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.339622641509434): 0.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.132075471698113): 0.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.150943396226415): 0.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.0754716981132075): 0.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.09433962264151): 2.094340
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.528301886792453): 0.528302
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.867924528301887): 0.867925
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.47169811320755): 1.471698
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.30188679245283): 0.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.132075471698113): 0.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.320754716981132): 0.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.679245283018868): 0.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.150943396226415): 0.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.339622641509434): 0.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.11320754716981): 1.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.83018867924528): 1.830189
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.169811320754717): 0.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')

## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.169811320754717): 0.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.377358490566038): 0.377358
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.264150943396226): 0.264151
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.13207547169811): 1.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.132075471698113): 0.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.226415094339623): 0.226415
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.54716981132075): 1.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.943396226415094): 0.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.207547169811321): 0.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.622641509433962): 0.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.62264150943396): 1.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.584905660377358): 0.584906
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.490566037735849): 0.490566
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.0754716981132075): 0.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.18867924528302): 1.188679
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.943396226415094): 0.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.56603773584906): 2.566038
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.339622641509434): 0.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.358490566037736): 0.358491
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 4.11320754716981): 4.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.660377358490566): 0.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.49056603773585): 3.490566
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.830188679245283): 0.830189
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.11320754716981): 3.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.11320754716981): 1.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.56603773584906): 1.566038
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.24528301886792): 2.245283
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.32075471698113): 3.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.9622641509434): 2.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.09433962264151): 2.094340
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 6.05660377358491): 6.056604
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 16.0188679245283): 16.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 18.3396226415094): 18.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 39.4528301886792): 39.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.4905660377358): 44.490566
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 31.4905660377358): 31.490566
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 49.2641509433962): 49.264151
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 53.7735849056604): 53.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 63.4528301886792): 63.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 49.9622641509434): 49.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 47.0754716981132): 47.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 52.1509433962264): 52.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 39.3396226415094): 39.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.0188679245283): 44.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.1698113207547): 44.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 37.3584905660377): 37.358491
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 49.0377358490566): 49.037736
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.811320754717): 43.811321
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.377358490566): 44.377358
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 50.5094339622642): 50.509434
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 54.5094339622642): 54.509434
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 49.9245283018868): 49.924528
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 50.9811320754717): 50.981132
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 55.6792452830189): 55.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.3207547169811): 44.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 52.2641509433962): 52.264151
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 69.5471698113208): 69.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 57.8490566037736): 57.849057
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 56.1509433962264): 56.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 73.377358490566): 73.377358
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 68.2075471698113): 68.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 129.433962264151): 129.433962
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 157.528301886792): 157.528302
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 171.150943396226): 171.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 155.396226415094): 155.396226
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 177.301886792453): 177.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 206.169811320755): 206.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 195.924528301887): 195.924528
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 179.566037735849): 179.566038
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 183.396226415094): 183.396226
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 167.018867924528): 167.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 143.452830188679): 143.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 124.037735849057): 124.037736
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 109.11320754717): 109.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 108.11320754717): 108.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 103.716981132075): 103.716981
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 95.9622641509434): 95.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 66.2075471698113): 66.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 45.2264150943396): 45.226415
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 24.7924528301887): 24.792453
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 38.7547169811321): 38.754717
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 34.9811320754717): 34.981132
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 21.0566037735849): 21.056604
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 40.5660377358491): 40.566038
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 26.9811320754717): 26.981132
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 42.4150943396226): 42.415094
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 52.6603773584906): 52.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 38.9245283018868): 38.924528
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 50.7924528301887): 50.792453
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.2830188679245): 44.283019
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 37.4150943396226): 37.415094
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 34.6981132075472): 34.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 28.3396226415094): 28.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 25.0943396226415): 25.094340
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 31.9433962264151): 31.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 31.3584905660377): 31.358491
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 29.6792452830189): 29.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 21.3207547169811): 21.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 25.5471698113208): 25.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 28.377358490566): 28.377358
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 26.4716981132075): 26.471698
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 33.4339622641509): 33.433962
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 49.9811320754717): 49.981132
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 42.0377358490566): 42.037736
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.6037735849057): 44.603774
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 46.0377358490566): 46.037736
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 59.188679245283): 59.188679
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 63.8679245283019): 63.867925
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 87.6981132075472): 87.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 94.8490566037736): 94.849057
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 92.7735849056604): 92.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 63.3962264150943): 63.396226
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 50.1698113207547): 50.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 54.4716981132075): 54.471698
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 32.4150943396226): 32.415094
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 26.5283018867925): 26.528302
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 37.7358490566038): 37.735849
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 45.0566037735849): 45.056604
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 67.2830188679245): 67.283019
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 42.3396226415094): 42.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 39.8867924528302): 39.886792
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.2641509433962): 43.264151
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 40.9811320754717): 40.981132
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 46.2452830188679): 46.245283
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 56.4339622641509): 56.433962
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 42.7547169811321): 42.754717
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 25.1320754716981): 25.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 39.9622641509434): 39.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 53.5471698113208): 53.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 47.3207547169811): 47.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 60.811320754717): 60.811321
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 55.7547169811321): 55.754717
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 51.9622641509434): 51.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.5849056603774): 43.584906
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 48.6981132075472): 48.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 35.4716981132075): 35.471698
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 37.5471698113208): 37.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 41.8490566037736): 41.849057
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 27.5094339622642): 27.509434
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 17.1132075471698): 17.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 26.0754716981132): 26.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.622641509434): 43.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.7735849056604): 43.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 30.0188679245283): 30.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 36.0754716981132): 36.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 35.4905660377358): 35.490566
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 38.8490566037736): 38.849057
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 45.9622641509434): 45.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 47.7547169811321): 47.754717
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 48.1320754716981): 48.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 65.3207547169811): 65.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 82.9056603773585): 82.905660
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 98.6603773584906): 98.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 102.11320754717): 102.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 83.9622641509434): 83.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 62.1320754716981): 62.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 64.1320754716981): 64.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 74.5471698113208): 74.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 63.1698113207547): 63.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 56.9056603773585): 56.905660
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 59.7735849056604): 59.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.8679245283019): 43.867925
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 38.5660377358491): 38.566038
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.6603773584906): 44.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 45.4528301886792): 45.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 46.2075471698113): 46.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.6792452830189): 43.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 46.622641509434): 46.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 56.3018867924528): 56.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 50.7169811320755): 50.716981
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 61.2264150943396): 61.226415
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 72.7169811320755): 72.716981
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 78.9433962264151): 78.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 68.9433962264151): 68.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 59.6603773584906): 59.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 75.0943396226415): 75.094340
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 56.5094339622642): 56.509434
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 34.7735849056604): 34.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 37.4528301886792): 37.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 40.6792452830189): 40.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 58.0188679245283): 58.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 74.6981132075472): 74.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 85.3207547169811): 85.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 59.2641509433962): 59.264151
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 67.7735849056604): 67.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 77.6981132075472): 77.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 74.2452830188679): 74.245283
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 85.3396226415094): 85.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 99.4528301886792): 99.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 86.5849056603774): 86.584906
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 85.6037735849057): 85.603774
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 84.8679245283019): 84.867925
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 77.8301886792453): 77.830189
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 58.0377358490566): 58.037736
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 53.3584905660377): 53.358491
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 36.3207547169811): 36.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 20.7169811320755): 20.716981
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 27.3962264150943): 27.396226
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 40.0188679245283): 40.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 30.2075471698113): 30.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 25.5471698113208): 25.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 45.6603773584906): 45.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 33.5283018867925): 33.528302
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 19.622641509434): 19.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 19.0188679245283): 19.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 19.3396226415094): 19.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 33.3396226415094): 33.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 26.811320754717): 26.811321
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 21.1698113207547): 21.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 27.3018867924528): 27.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 21.3396226415094): 21.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 19.5471698113208): 19.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 21.3207547169811): 21.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 32.3018867924528): 32.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 20.1509433962264): 20.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 15.9433962264151): 15.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 17.2264150943396): 17.226415
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 23.4528301886792): 23.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 19.2452830188679): 19.245283
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 12.4528301886792): 12.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 8.0188679245283): 8.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 14.6603773584906): 14.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 16.3018867924528): 16.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 8.67924528301887): 8.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 7.79245283018868): 7.792453
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 8.13207547169811): 8.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.62264150943396): 2.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.45283018867925): 1.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.67924528301887): 3.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 4.81132075471698): 4.811321
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 8.50943396226415): 8.509434
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 7.07547169811321): 7.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 8.69811320754717): 8.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 9.75471698113208): 9.754717
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.20754716981132): 2.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.320754716981132): 0.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.113207547169811): 0.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.60377358490566): 1.603774
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 4.60377358490566): 4.603774
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.30188679245283): 3.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.84905660377358): 2.849057
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.830188679245283): 0.830189
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.962264150943396): 0.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.58490566037736): 1.584906
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.60377358490566): 2.603774
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 4.69811320754717): 4.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.30188679245283): 3.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.641509433962264): 0.641509
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.226415094339623): 0.226415
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.07547169811321): 1.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.71698113207547): 1.716981
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.339622641509434): 0.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.132075471698113): 0.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.150943396226415): 0.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.0754716981132075): 0.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.09433962264151): 2.094340
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.528301886792453): 0.528302
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.867924528301887): 0.867925
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.47169811320755): 1.471698
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.30188679245283): 0.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.132075471698113): 0.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.320754716981132): 0.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.679245283018868): 0.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.150943396226415): 0.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.339622641509434): 0.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.11320754716981): 1.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.83018867924528): 1.830189
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.169811320754717): 0.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')

## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.169811320754717): 0.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.377358490566038): 0.377358
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.264150943396226): 0.264151
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.13207547169811): 1.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.132075471698113): 0.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.226415094339623): 0.226415
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.54716981132075): 1.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.943396226415094): 0.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.207547169811321): 0.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.622641509433962): 0.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.62264150943396): 1.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.584905660377358): 0.584906
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.490566037735849): 0.490566
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.0754716981132075): 0.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.18867924528302): 1.188679
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.943396226415094): 0.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.56603773584906): 2.566038
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.339622641509434): 0.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.358490566037736): 0.358491
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 4.11320754716981): 4.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.660377358490566): 0.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.49056603773585): 3.490566
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.830188679245283): 0.830189
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.11320754716981): 3.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.11320754716981): 1.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.56603773584906): 1.566038
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.24528301886792): 2.245283
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.32075471698113): 3.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.9622641509434): 2.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.09433962264151): 2.094340
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 6.05660377358491): 6.056604
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 16.0188679245283): 16.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 18.3396226415094): 18.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 39.4528301886792): 39.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.4905660377358): 44.490566
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 31.4905660377358): 31.490566
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 49.2641509433962): 49.264151
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 53.7735849056604): 53.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 63.4528301886792): 63.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 49.9622641509434): 49.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 47.0754716981132): 47.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 52.1509433962264): 52.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 39.3396226415094): 39.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.0188679245283): 44.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.1698113207547): 44.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 37.3584905660377): 37.358491
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 49.0377358490566): 49.037736
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.811320754717): 43.811321
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.377358490566): 44.377358
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 50.5094339622642): 50.509434
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 54.5094339622642): 54.509434
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 49.9245283018868): 49.924528
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 50.9811320754717): 50.981132
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 55.6792452830189): 55.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.3207547169811): 44.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 52.2641509433962): 52.264151
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 69.5471698113208): 69.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 57.8490566037736): 57.849057
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 56.1509433962264): 56.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 73.377358490566): 73.377358
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 68.2075471698113): 68.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 129.433962264151): 129.433962
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 157.528301886792): 157.528302
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 171.150943396226): 171.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 155.396226415094): 155.396226
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 177.301886792453): 177.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 206.169811320755): 206.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 195.924528301887): 195.924528
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 179.566037735849): 179.566038
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 183.396226415094): 183.396226
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 167.018867924528): 167.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 143.452830188679): 143.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 124.037735849057): 124.037736
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 109.11320754717): 109.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 108.11320754717): 108.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 103.716981132075): 103.716981
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 95.9622641509434): 95.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 66.2075471698113): 66.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 45.2264150943396): 45.226415
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 24.7924528301887): 24.792453
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 38.7547169811321): 38.754717
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 34.9811320754717): 34.981132
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 21.0566037735849): 21.056604
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 40.5660377358491): 40.566038
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 26.9811320754717): 26.981132
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 42.4150943396226): 42.415094
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 52.6603773584906): 52.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 38.9245283018868): 38.924528
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 50.7924528301887): 50.792453
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.2830188679245): 44.283019
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 37.4150943396226): 37.415094
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 34.6981132075472): 34.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 28.3396226415094): 28.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 25.0943396226415): 25.094340
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 31.9433962264151): 31.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 31.3584905660377): 31.358491
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 29.6792452830189): 29.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 21.3207547169811): 21.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 25.5471698113208): 25.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 28.377358490566): 28.377358
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 26.4716981132075): 26.471698
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 33.4339622641509): 33.433962
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 49.9811320754717): 49.981132
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 42.0377358490566): 42.037736
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.6037735849057): 44.603774
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 46.0377358490566): 46.037736
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 59.188679245283): 59.188679
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 63.8679245283019): 63.867925
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 87.6981132075472): 87.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 94.8490566037736): 94.849057
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 92.7735849056604): 92.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 63.3962264150943): 63.396226
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 50.1698113207547): 50.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 54.4716981132075): 54.471698
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 32.4150943396226): 32.415094
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 26.5283018867925): 26.528302
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 37.7358490566038): 37.735849
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 45.0566037735849): 45.056604
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 67.2830188679245): 67.283019
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 42.3396226415094): 42.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 39.8867924528302): 39.886792
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.2641509433962): 43.264151
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 40.9811320754717): 40.981132
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 46.2452830188679): 46.245283
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 56.4339622641509): 56.433962
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 42.7547169811321): 42.754717
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 25.1320754716981): 25.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 39.9622641509434): 39.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 53.5471698113208): 53.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 47.3207547169811): 47.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 60.811320754717): 60.811321
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 55.7547169811321): 55.754717
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 51.9622641509434): 51.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.5849056603774): 43.584906
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 48.6981132075472): 48.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 35.4716981132075): 35.471698
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 37.5471698113208): 37.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 41.8490566037736): 41.849057
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 27.5094339622642): 27.509434
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 17.1132075471698): 17.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 26.0754716981132): 26.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.622641509434): 43.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.7735849056604): 43.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 30.0188679245283): 30.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 36.0754716981132): 36.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 35.4905660377358): 35.490566
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 38.8490566037736): 38.849057
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 45.9622641509434): 45.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 47.7547169811321): 47.754717
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 48.1320754716981): 48.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 65.3207547169811): 65.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 82.9056603773585): 82.905660
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 98.6603773584906): 98.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 102.11320754717): 102.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 83.9622641509434): 83.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 62.1320754716981): 62.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 64.1320754716981): 64.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 74.5471698113208): 74.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 63.1698113207547): 63.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 56.9056603773585): 56.905660
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 59.7735849056604): 59.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.8679245283019): 43.867925
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 38.5660377358491): 38.566038
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.6603773584906): 44.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 45.4528301886792): 45.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 46.2075471698113): 46.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.6792452830189): 43.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 46.622641509434): 46.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 56.3018867924528): 56.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 50.7169811320755): 50.716981
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 61.2264150943396): 61.226415
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 72.7169811320755): 72.716981
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 78.9433962264151): 78.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 68.9433962264151): 68.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 59.6603773584906): 59.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 75.0943396226415): 75.094340
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 56.5094339622642): 56.509434
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 34.7735849056604): 34.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 37.4528301886792): 37.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 40.6792452830189): 40.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 58.0188679245283): 58.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 74.6981132075472): 74.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 85.3207547169811): 85.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 59.2641509433962): 59.264151
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 67.7735849056604): 67.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 77.6981132075472): 77.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 74.2452830188679): 74.245283
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 85.3396226415094): 85.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 99.4528301886792): 99.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 86.5849056603774): 86.584906
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 85.6037735849057): 85.603774
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 84.8679245283019): 84.867925
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 77.8301886792453): 77.830189
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 58.0377358490566): 58.037736
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 53.3584905660377): 53.358491
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 36.3207547169811): 36.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 20.7169811320755): 20.716981
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 27.3962264150943): 27.396226
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 40.0188679245283): 40.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 30.2075471698113): 30.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 25.5471698113208): 25.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 45.6603773584906): 45.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 33.5283018867925): 33.528302
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 19.622641509434): 19.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 19.0188679245283): 19.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 19.3396226415094): 19.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 33.3396226415094): 33.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 26.811320754717): 26.811321
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 21.1698113207547): 21.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 27.3018867924528): 27.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 21.3396226415094): 21.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 19.5471698113208): 19.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 21.3207547169811): 21.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 32.3018867924528): 32.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 20.1509433962264): 20.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 15.9433962264151): 15.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 17.2264150943396): 17.226415
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 23.4528301886792): 23.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 19.2452830188679): 19.245283
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 12.4528301886792): 12.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 8.0188679245283): 8.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 14.6603773584906): 14.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 16.3018867924528): 16.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 8.67924528301887): 8.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 7.79245283018868): 7.792453
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 8.13207547169811): 8.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.62264150943396): 2.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.45283018867925): 1.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.67924528301887): 3.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 4.81132075471698): 4.811321
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 8.50943396226415): 8.509434
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 7.07547169811321): 7.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 8.69811320754717): 8.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 9.75471698113208): 9.754717
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.20754716981132): 2.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.320754716981132): 0.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.113207547169811): 0.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.60377358490566): 1.603774
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 4.60377358490566): 4.603774
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.30188679245283): 3.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.84905660377358): 2.849057
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.830188679245283): 0.830189
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.962264150943396): 0.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.58490566037736): 1.584906
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.60377358490566): 2.603774
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 4.69811320754717): 4.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.30188679245283): 3.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.641509433962264): 0.641509
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.226415094339623): 0.226415
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.07547169811321): 1.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.71698113207547): 1.716981
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.339622641509434): 0.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.132075471698113): 0.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.150943396226415): 0.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.0754716981132075): 0.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.09433962264151): 2.094340
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.528301886792453): 0.528302
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.867924528301887): 0.867925
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.47169811320755): 1.471698
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.30188679245283): 0.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.132075471698113): 0.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.320754716981132): 0.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.679245283018868): 0.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.150943396226415): 0.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.339622641509434): 0.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.11320754716981): 1.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.83018867924528): 1.830189
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.169811320754717): 0.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')

## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.169811320754717): 0.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.377358490566038): 0.377358
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.264150943396226): 0.264151
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.13207547169811): 1.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.132075471698113): 0.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.226415094339623): 0.226415
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.54716981132075): 1.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.943396226415094): 0.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.207547169811321): 0.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.622641509433962): 0.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.62264150943396): 1.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.584905660377358): 0.584906
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.490566037735849): 0.490566
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.0754716981132075): 0.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.18867924528302): 1.188679
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.943396226415094): 0.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.56603773584906): 2.566038
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.339622641509434): 0.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.358490566037736): 0.358491
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 4.11320754716981): 4.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.660377358490566): 0.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.49056603773585): 3.490566
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.830188679245283): 0.830189
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.11320754716981): 3.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.11320754716981): 1.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.56603773584906): 1.566038
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.24528301886792): 2.245283
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.32075471698113): 3.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.9622641509434): 2.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.09433962264151): 2.094340
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 6.05660377358491): 6.056604
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 16.0188679245283): 16.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 18.3396226415094): 18.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 39.4528301886792): 39.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.4905660377358): 44.490566
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 31.4905660377358): 31.490566
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 49.2641509433962): 49.264151
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 53.7735849056604): 53.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 63.4528301886792): 63.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 49.9622641509434): 49.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 47.0754716981132): 47.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 52.1509433962264): 52.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 39.3396226415094): 39.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.0188679245283): 44.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.1698113207547): 44.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 37.3584905660377): 37.358491
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 49.0377358490566): 49.037736
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.811320754717): 43.811321
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.377358490566): 44.377358
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 50.5094339622642): 50.509434
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 54.5094339622642): 54.509434
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 49.9245283018868): 49.924528
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 50.9811320754717): 50.981132
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 55.6792452830189): 55.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.3207547169811): 44.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 52.2641509433962): 52.264151
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 69.5471698113208): 69.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 57.8490566037736): 57.849057
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 56.1509433962264): 56.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 73.377358490566): 73.377358
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 68.2075471698113): 68.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 129.433962264151): 129.433962
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 157.528301886792): 157.528302
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 171.150943396226): 171.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 155.396226415094): 155.396226
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 177.301886792453): 177.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 206.169811320755): 206.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 195.924528301887): 195.924528
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 179.566037735849): 179.566038
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 183.396226415094): 183.396226
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 167.018867924528): 167.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 143.452830188679): 143.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 124.037735849057): 124.037736
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 109.11320754717): 109.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 108.11320754717): 108.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 103.716981132075): 103.716981
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 95.9622641509434): 95.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 66.2075471698113): 66.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 45.2264150943396): 45.226415
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 24.7924528301887): 24.792453
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 38.7547169811321): 38.754717
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 34.9811320754717): 34.981132
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 21.0566037735849): 21.056604
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 40.5660377358491): 40.566038
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 26.9811320754717): 26.981132
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 42.4150943396226): 42.415094
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 52.6603773584906): 52.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 38.9245283018868): 38.924528
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 50.7924528301887): 50.792453
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.2830188679245): 44.283019
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 37.4150943396226): 37.415094
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 34.6981132075472): 34.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 28.3396226415094): 28.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 25.0943396226415): 25.094340
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 31.9433962264151): 31.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 31.3584905660377): 31.358491
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 29.6792452830189): 29.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 21.3207547169811): 21.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 25.5471698113208): 25.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 28.377358490566): 28.377358
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 26.4716981132075): 26.471698
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 33.4339622641509): 33.433962
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 49.9811320754717): 49.981132
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 42.0377358490566): 42.037736
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.6037735849057): 44.603774
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 46.0377358490566): 46.037736
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 59.188679245283): 59.188679
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 63.8679245283019): 63.867925
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 87.6981132075472): 87.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 94.8490566037736): 94.849057
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 92.7735849056604): 92.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 63.3962264150943): 63.396226
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 50.1698113207547): 50.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 54.4716981132075): 54.471698
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 32.4150943396226): 32.415094
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 26.5283018867925): 26.528302
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 37.7358490566038): 37.735849
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 45.0566037735849): 45.056604
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 67.2830188679245): 67.283019
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 42.3396226415094): 42.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 39.8867924528302): 39.886792
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.2641509433962): 43.264151
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 40.9811320754717): 40.981132
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 46.2452830188679): 46.245283
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 56.4339622641509): 56.433962
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 42.7547169811321): 42.754717
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 25.1320754716981): 25.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 39.9622641509434): 39.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 53.5471698113208): 53.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 47.3207547169811): 47.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 60.811320754717): 60.811321
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 55.7547169811321): 55.754717
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 51.9622641509434): 51.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.5849056603774): 43.584906
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 48.6981132075472): 48.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 35.4716981132075): 35.471698
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 37.5471698113208): 37.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 41.8490566037736): 41.849057
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 27.5094339622642): 27.509434
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 17.1132075471698): 17.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 26.0754716981132): 26.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.622641509434): 43.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.7735849056604): 43.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 30.0188679245283): 30.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 36.0754716981132): 36.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 35.4905660377358): 35.490566
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 38.8490566037736): 38.849057
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 45.9622641509434): 45.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 47.7547169811321): 47.754717
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 48.1320754716981): 48.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 65.3207547169811): 65.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 82.9056603773585): 82.905660
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 98.6603773584906): 98.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 102.11320754717): 102.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 83.9622641509434): 83.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 62.1320754716981): 62.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 64.1320754716981): 64.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 74.5471698113208): 74.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 63.1698113207547): 63.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 56.9056603773585): 56.905660
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 59.7735849056604): 59.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.8679245283019): 43.867925
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 38.5660377358491): 38.566038
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.6603773584906): 44.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 45.4528301886792): 45.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 46.2075471698113): 46.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.6792452830189): 43.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 46.622641509434): 46.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 56.3018867924528): 56.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 50.7169811320755): 50.716981
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 61.2264150943396): 61.226415
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 72.7169811320755): 72.716981
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 78.9433962264151): 78.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 68.9433962264151): 68.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 59.6603773584906): 59.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 75.0943396226415): 75.094340
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 56.5094339622642): 56.509434
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 34.7735849056604): 34.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 37.4528301886792): 37.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 40.6792452830189): 40.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 58.0188679245283): 58.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 74.6981132075472): 74.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 85.3207547169811): 85.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 59.2641509433962): 59.264151
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 67.7735849056604): 67.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 77.6981132075472): 77.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 74.2452830188679): 74.245283
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 85.3396226415094): 85.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 99.4528301886792): 99.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 86.5849056603774): 86.584906
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 85.6037735849057): 85.603774
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 84.8679245283019): 84.867925
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 77.8301886792453): 77.830189
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 58.0377358490566): 58.037736
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 53.3584905660377): 53.358491
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 36.3207547169811): 36.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 20.7169811320755): 20.716981
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 27.3962264150943): 27.396226
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 40.0188679245283): 40.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 30.2075471698113): 30.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 25.5471698113208): 25.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 45.6603773584906): 45.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 33.5283018867925): 33.528302
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 19.622641509434): 19.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 19.0188679245283): 19.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 19.3396226415094): 19.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 33.3396226415094): 33.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 26.811320754717): 26.811321
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 21.1698113207547): 21.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 27.3018867924528): 27.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 21.3396226415094): 21.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 19.5471698113208): 19.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 21.3207547169811): 21.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 32.3018867924528): 32.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 20.1509433962264): 20.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 15.9433962264151): 15.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 17.2264150943396): 17.226415
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 23.4528301886792): 23.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 19.2452830188679): 19.245283
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 12.4528301886792): 12.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 8.0188679245283): 8.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 14.6603773584906): 14.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 16.3018867924528): 16.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 8.67924528301887): 8.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 7.79245283018868): 7.792453
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 8.13207547169811): 8.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.62264150943396): 2.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.45283018867925): 1.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.67924528301887): 3.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 4.81132075471698): 4.811321
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 8.50943396226415): 8.509434
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 7.07547169811321): 7.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 8.69811320754717): 8.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 9.75471698113208): 9.754717
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.20754716981132): 2.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.320754716981132): 0.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.113207547169811): 0.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.60377358490566): 1.603774
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 4.60377358490566): 4.603774
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.30188679245283): 3.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.84905660377358): 2.849057
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.830188679245283): 0.830189
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.962264150943396): 0.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.58490566037736): 1.584906
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.60377358490566): 2.603774
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 4.69811320754717): 4.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.30188679245283): 3.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.641509433962264): 0.641509
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.226415094339623): 0.226415
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.07547169811321): 1.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.71698113207547): 1.716981
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.339622641509434): 0.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.132075471698113): 0.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.150943396226415): 0.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.0754716981132075): 0.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.09433962264151): 2.094340
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.528301886792453): 0.528302
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.867924528301887): 0.867925
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.47169811320755): 1.471698
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.30188679245283): 0.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.132075471698113): 0.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.320754716981132): 0.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.679245283018868): 0.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.150943396226415): 0.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.339622641509434): 0.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.11320754716981): 1.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.83018867924528): 1.830189
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.169811320754717): 0.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')

## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.169811320754717): 0.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.377358490566038): 0.377358
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.264150943396226): 0.264151
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.13207547169811): 1.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.132075471698113): 0.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.226415094339623): 0.226415
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.54716981132075): 1.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.943396226415094): 0.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.207547169811321): 0.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.622641509433962): 0.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.62264150943396): 1.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.584905660377358): 0.584906
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.490566037735849): 0.490566
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.0754716981132075): 0.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.18867924528302): 1.188679
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.943396226415094): 0.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.56603773584906): 2.566038
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.339622641509434): 0.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.358490566037736): 0.358491
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 4.11320754716981): 4.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.660377358490566): 0.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.49056603773585): 3.490566
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.830188679245283): 0.830189
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.11320754716981): 3.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.11320754716981): 1.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.56603773584906): 1.566038
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.24528301886792): 2.245283
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.32075471698113): 3.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.9622641509434): 2.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.09433962264151): 2.094340
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 6.05660377358491): 6.056604
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 16.0188679245283): 16.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 18.3396226415094): 18.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 39.4528301886792): 39.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.4905660377358): 44.490566
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 31.4905660377358): 31.490566
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 49.2641509433962): 49.264151
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 53.7735849056604): 53.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 63.4528301886792): 63.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 49.9622641509434): 49.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 47.0754716981132): 47.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 52.1509433962264): 52.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 39.3396226415094): 39.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.0188679245283): 44.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.1698113207547): 44.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 37.3584905660377): 37.358491
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 49.0377358490566): 49.037736
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.811320754717): 43.811321
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.377358490566): 44.377358
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 50.5094339622642): 50.509434
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 54.5094339622642): 54.509434
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 49.9245283018868): 49.924528
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 50.9811320754717): 50.981132
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 55.6792452830189): 55.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.3207547169811): 44.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 52.2641509433962): 52.264151
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 69.5471698113208): 69.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 57.8490566037736): 57.849057
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 56.1509433962264): 56.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 73.377358490566): 73.377358
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 68.2075471698113): 68.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 129.433962264151): 129.433962
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 157.528301886792): 157.528302
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 171.150943396226): 171.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 155.396226415094): 155.396226
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 177.301886792453): 177.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 206.169811320755): 206.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 195.924528301887): 195.924528
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 179.566037735849): 179.566038
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 183.396226415094): 183.396226
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 167.018867924528): 167.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 143.452830188679): 143.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 124.037735849057): 124.037736
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 109.11320754717): 109.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 108.11320754717): 108.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 103.716981132075): 103.716981
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 95.9622641509434): 95.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 66.2075471698113): 66.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 45.2264150943396): 45.226415
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 24.7924528301887): 24.792453
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 38.7547169811321): 38.754717
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 34.9811320754717): 34.981132
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 21.0566037735849): 21.056604
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 40.5660377358491): 40.566038
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 26.9811320754717): 26.981132
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 42.4150943396226): 42.415094
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 52.6603773584906): 52.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 38.9245283018868): 38.924528
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 50.7924528301887): 50.792453
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.2830188679245): 44.283019
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 37.4150943396226): 37.415094
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 34.6981132075472): 34.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 28.3396226415094): 28.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 25.0943396226415): 25.094340
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 31.9433962264151): 31.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 31.3584905660377): 31.358491
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 29.6792452830189): 29.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 21.3207547169811): 21.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 25.5471698113208): 25.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 28.377358490566): 28.377358
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 26.4716981132075): 26.471698
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 33.4339622641509): 33.433962
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 49.9811320754717): 49.981132
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 42.0377358490566): 42.037736
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.6037735849057): 44.603774
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 46.0377358490566): 46.037736
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 59.188679245283): 59.188679
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 63.8679245283019): 63.867925
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 87.6981132075472): 87.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 94.8490566037736): 94.849057
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 92.7735849056604): 92.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 63.3962264150943): 63.396226
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 50.1698113207547): 50.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 54.4716981132075): 54.471698
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 32.4150943396226): 32.415094
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 26.5283018867925): 26.528302
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 37.7358490566038): 37.735849
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 45.0566037735849): 45.056604
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 67.2830188679245): 67.283019
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 42.3396226415094): 42.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 39.8867924528302): 39.886792
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.2641509433962): 43.264151
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 40.9811320754717): 40.981132
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 46.2452830188679): 46.245283
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 56.4339622641509): 56.433962
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 42.7547169811321): 42.754717
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 25.1320754716981): 25.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 39.9622641509434): 39.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 53.5471698113208): 53.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 47.3207547169811): 47.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 60.811320754717): 60.811321
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 55.7547169811321): 55.754717
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 51.9622641509434): 51.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.5849056603774): 43.584906
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 48.6981132075472): 48.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 35.4716981132075): 35.471698
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 37.5471698113208): 37.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 41.8490566037736): 41.849057
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 27.5094339622642): 27.509434
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 17.1132075471698): 17.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 26.0754716981132): 26.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.622641509434): 43.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.7735849056604): 43.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 30.0188679245283): 30.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 36.0754716981132): 36.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 35.4905660377358): 35.490566
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 38.8490566037736): 38.849057
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 45.9622641509434): 45.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 47.7547169811321): 47.754717
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 48.1320754716981): 48.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 65.3207547169811): 65.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 82.9056603773585): 82.905660
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 98.6603773584906): 98.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 102.11320754717): 102.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 83.9622641509434): 83.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 62.1320754716981): 62.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 64.1320754716981): 64.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 74.5471698113208): 74.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 63.1698113207547): 63.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 56.9056603773585): 56.905660
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 59.7735849056604): 59.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.8679245283019): 43.867925
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 38.5660377358491): 38.566038
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.6603773584906): 44.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 45.4528301886792): 45.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 46.2075471698113): 46.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.6792452830189): 43.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 46.622641509434): 46.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 56.3018867924528): 56.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 50.7169811320755): 50.716981
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 61.2264150943396): 61.226415
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 72.7169811320755): 72.716981
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 78.9433962264151): 78.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 68.9433962264151): 68.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 59.6603773584906): 59.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 75.0943396226415): 75.094340
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 56.5094339622642): 56.509434
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 34.7735849056604): 34.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 37.4528301886792): 37.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 40.6792452830189): 40.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 58.0188679245283): 58.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 74.6981132075472): 74.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 85.3207547169811): 85.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 59.2641509433962): 59.264151
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 67.7735849056604): 67.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 77.6981132075472): 77.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 74.2452830188679): 74.245283
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 85.3396226415094): 85.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 99.4528301886792): 99.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 86.5849056603774): 86.584906
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 85.6037735849057): 85.603774
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 84.8679245283019): 84.867925
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 77.8301886792453): 77.830189
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 58.0377358490566): 58.037736
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 53.3584905660377): 53.358491
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 36.3207547169811): 36.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 20.7169811320755): 20.716981
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 27.3962264150943): 27.396226
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 40.0188679245283): 40.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 30.2075471698113): 30.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 25.5471698113208): 25.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 45.6603773584906): 45.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 33.5283018867925): 33.528302
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 19.622641509434): 19.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 19.0188679245283): 19.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 19.3396226415094): 19.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 33.3396226415094): 33.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 26.811320754717): 26.811321
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 21.1698113207547): 21.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 27.3018867924528): 27.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 21.3396226415094): 21.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 19.5471698113208): 19.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 21.3207547169811): 21.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 32.3018867924528): 32.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 20.1509433962264): 20.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 15.9433962264151): 15.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 17.2264150943396): 17.226415
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 23.4528301886792): 23.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 19.2452830188679): 19.245283
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 12.4528301886792): 12.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 8.0188679245283): 8.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 14.6603773584906): 14.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 16.3018867924528): 16.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 8.67924528301887): 8.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 7.79245283018868): 7.792453
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 8.13207547169811): 8.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.62264150943396): 2.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.45283018867925): 1.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.67924528301887): 3.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 4.81132075471698): 4.811321
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 8.50943396226415): 8.509434
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 7.07547169811321): 7.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 8.69811320754717): 8.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 9.75471698113208): 9.754717
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.20754716981132): 2.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.320754716981132): 0.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.113207547169811): 0.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.60377358490566): 1.603774
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 4.60377358490566): 4.603774
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.30188679245283): 3.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.84905660377358): 2.849057
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.830188679245283): 0.830189
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.962264150943396): 0.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.58490566037736): 1.584906
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.60377358490566): 2.603774
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 4.69811320754717): 4.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.30188679245283): 3.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.641509433962264): 0.641509
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.226415094339623): 0.226415
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.07547169811321): 1.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.71698113207547): 1.716981
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.339622641509434): 0.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.132075471698113): 0.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.150943396226415): 0.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.0754716981132075): 0.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.09433962264151): 2.094340
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.528301886792453): 0.528302
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.867924528301887): 0.867925
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.47169811320755): 1.471698
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.30188679245283): 0.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.132075471698113): 0.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.320754716981132): 0.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.679245283018868): 0.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.150943396226415): 0.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.339622641509434): 0.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.11320754716981): 1.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.83018867924528): 1.830189
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.169811320754717): 0.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')

## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.169811320754717): 0.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.377358490566038): 0.377358
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.264150943396226): 0.264151
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.13207547169811): 1.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.132075471698113): 0.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.226415094339623): 0.226415
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.54716981132075): 1.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.943396226415094): 0.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.207547169811321): 0.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.622641509433962): 0.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.62264150943396): 1.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.584905660377358): 0.584906
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.490566037735849): 0.490566
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.0754716981132075): 0.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.18867924528302): 1.188679
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.943396226415094): 0.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.56603773584906): 2.566038
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.339622641509434): 0.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.358490566037736): 0.358491
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 4.11320754716981): 4.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.660377358490566): 0.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.49056603773585): 3.490566
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.830188679245283): 0.830189
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.11320754716981): 3.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.11320754716981): 1.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.56603773584906): 1.566038
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.24528301886792): 2.245283
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.32075471698113): 3.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.9622641509434): 2.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.09433962264151): 2.094340
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 6.05660377358491): 6.056604
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 16.0188679245283): 16.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 18.3396226415094): 18.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 39.4528301886792): 39.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.4905660377358): 44.490566
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 31.4905660377358): 31.490566
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 49.2641509433962): 49.264151
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 53.7735849056604): 53.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 63.4528301886792): 63.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 49.9622641509434): 49.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 47.0754716981132): 47.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 52.1509433962264): 52.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 39.3396226415094): 39.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.0188679245283): 44.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.1698113207547): 44.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 37.3584905660377): 37.358491
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 49.0377358490566): 49.037736
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.811320754717): 43.811321
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.377358490566): 44.377358
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 50.5094339622642): 50.509434
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 54.5094339622642): 54.509434
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 49.9245283018868): 49.924528
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 50.9811320754717): 50.981132
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 55.6792452830189): 55.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.3207547169811): 44.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 52.2641509433962): 52.264151
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 69.5471698113208): 69.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 57.8490566037736): 57.849057
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 56.1509433962264): 56.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 73.377358490566): 73.377358
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 68.2075471698113): 68.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 129.433962264151): 129.433962
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 157.528301886792): 157.528302
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 171.150943396226): 171.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 155.396226415094): 155.396226
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 177.301886792453): 177.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 206.169811320755): 206.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 195.924528301887): 195.924528
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 179.566037735849): 179.566038
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 183.396226415094): 183.396226
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 167.018867924528): 167.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 143.452830188679): 143.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 124.037735849057): 124.037736
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 109.11320754717): 109.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 108.11320754717): 108.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 103.716981132075): 103.716981
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 95.9622641509434): 95.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 66.2075471698113): 66.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 45.2264150943396): 45.226415
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 24.7924528301887): 24.792453
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 38.7547169811321): 38.754717
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 34.9811320754717): 34.981132
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 21.0566037735849): 21.056604
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 40.5660377358491): 40.566038
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 26.9811320754717): 26.981132
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 42.4150943396226): 42.415094
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 52.6603773584906): 52.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 38.9245283018868): 38.924528
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 50.7924528301887): 50.792453
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.2830188679245): 44.283019
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 37.4150943396226): 37.415094
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 34.6981132075472): 34.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 28.3396226415094): 28.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 25.0943396226415): 25.094340
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 31.9433962264151): 31.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 31.3584905660377): 31.358491
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 29.6792452830189): 29.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 21.3207547169811): 21.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 25.5471698113208): 25.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 28.377358490566): 28.377358
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 26.4716981132075): 26.471698
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 33.4339622641509): 33.433962
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 49.9811320754717): 49.981132
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 42.0377358490566): 42.037736
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.6037735849057): 44.603774
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 46.0377358490566): 46.037736
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 59.188679245283): 59.188679
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 63.8679245283019): 63.867925
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 87.6981132075472): 87.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 94.8490566037736): 94.849057
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 92.7735849056604): 92.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 63.3962264150943): 63.396226
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 50.1698113207547): 50.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 54.4716981132075): 54.471698
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 32.4150943396226): 32.415094
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 26.5283018867925): 26.528302
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 37.7358490566038): 37.735849
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 45.0566037735849): 45.056604
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 67.2830188679245): 67.283019
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 42.3396226415094): 42.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 39.8867924528302): 39.886792
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.2641509433962): 43.264151
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 40.9811320754717): 40.981132
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 46.2452830188679): 46.245283
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 56.4339622641509): 56.433962
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 42.7547169811321): 42.754717
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 25.1320754716981): 25.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 39.9622641509434): 39.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 53.5471698113208): 53.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 47.3207547169811): 47.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 60.811320754717): 60.811321
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 55.7547169811321): 55.754717
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 51.9622641509434): 51.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.5849056603774): 43.584906
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 48.6981132075472): 48.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 35.4716981132075): 35.471698
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 37.5471698113208): 37.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 41.8490566037736): 41.849057
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 27.5094339622642): 27.509434
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 17.1132075471698): 17.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 26.0754716981132): 26.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.622641509434): 43.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.7735849056604): 43.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 30.0188679245283): 30.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 36.0754716981132): 36.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 35.4905660377358): 35.490566
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 38.8490566037736): 38.849057
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 45.9622641509434): 45.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 47.7547169811321): 47.754717
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 48.1320754716981): 48.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 65.3207547169811): 65.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 82.9056603773585): 82.905660
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 98.6603773584906): 98.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 102.11320754717): 102.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 83.9622641509434): 83.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 62.1320754716981): 62.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 64.1320754716981): 64.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 74.5471698113208): 74.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 63.1698113207547): 63.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 56.9056603773585): 56.905660
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 59.7735849056604): 59.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.8679245283019): 43.867925
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 38.5660377358491): 38.566038
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.6603773584906): 44.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 45.4528301886792): 45.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 46.2075471698113): 46.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.6792452830189): 43.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 46.622641509434): 46.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 56.3018867924528): 56.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 50.7169811320755): 50.716981
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 61.2264150943396): 61.226415
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 72.7169811320755): 72.716981
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 78.9433962264151): 78.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 68.9433962264151): 68.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 59.6603773584906): 59.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 75.0943396226415): 75.094340
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 56.5094339622642): 56.509434
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 34.7735849056604): 34.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 37.4528301886792): 37.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 40.6792452830189): 40.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 58.0188679245283): 58.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 74.6981132075472): 74.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 85.3207547169811): 85.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 59.2641509433962): 59.264151
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 67.7735849056604): 67.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 77.6981132075472): 77.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 74.2452830188679): 74.245283
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 85.3396226415094): 85.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 99.4528301886792): 99.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 86.5849056603774): 86.584906
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 85.6037735849057): 85.603774
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 84.8679245283019): 84.867925
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 77.8301886792453): 77.830189
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 58.0377358490566): 58.037736
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 53.3584905660377): 53.358491
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 36.3207547169811): 36.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 20.7169811320755): 20.716981
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 27.3962264150943): 27.396226
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 40.0188679245283): 40.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 30.2075471698113): 30.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 25.5471698113208): 25.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 45.6603773584906): 45.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 33.5283018867925): 33.528302
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 19.622641509434): 19.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 19.0188679245283): 19.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 19.3396226415094): 19.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 33.3396226415094): 33.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 26.811320754717): 26.811321
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 21.1698113207547): 21.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 27.3018867924528): 27.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 21.3396226415094): 21.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 19.5471698113208): 19.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 21.3207547169811): 21.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 32.3018867924528): 32.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 20.1509433962264): 20.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 15.9433962264151): 15.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 17.2264150943396): 17.226415
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 23.4528301886792): 23.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 19.2452830188679): 19.245283
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 12.4528301886792): 12.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 8.0188679245283): 8.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 14.6603773584906): 14.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 16.3018867924528): 16.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 8.67924528301887): 8.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 7.79245283018868): 7.792453
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 8.13207547169811): 8.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.62264150943396): 2.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.45283018867925): 1.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.67924528301887): 3.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 4.81132075471698): 4.811321
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 8.50943396226415): 8.509434
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 7.07547169811321): 7.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 8.69811320754717): 8.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 9.75471698113208): 9.754717
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.20754716981132): 2.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.320754716981132): 0.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.113207547169811): 0.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.60377358490566): 1.603774
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 4.60377358490566): 4.603774
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.30188679245283): 3.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.84905660377358): 2.849057
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.830188679245283): 0.830189
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.962264150943396): 0.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.58490566037736): 1.584906
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.60377358490566): 2.603774
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 4.69811320754717): 4.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.30188679245283): 3.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.641509433962264): 0.641509
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.226415094339623): 0.226415
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.07547169811321): 1.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.71698113207547): 1.716981
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.339622641509434): 0.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.132075471698113): 0.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.150943396226415): 0.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.0754716981132075): 0.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.09433962264151): 2.094340
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.528301886792453): 0.528302
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.867924528301887): 0.867925
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.47169811320755): 1.471698
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.30188679245283): 0.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.132075471698113): 0.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.320754716981132): 0.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.679245283018868): 0.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.150943396226415): 0.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.339622641509434): 0.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.11320754716981): 1.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.83018867924528): 1.830189
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.169811320754717): 0.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')

## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.169811320754717): 0.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.377358490566038): 0.377358
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.264150943396226): 0.264151
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.13207547169811): 1.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.132075471698113): 0.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.226415094339623): 0.226415
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.54716981132075): 1.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.943396226415094): 0.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.207547169811321): 0.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.622641509433962): 0.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.62264150943396): 1.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.584905660377358): 0.584906
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.490566037735849): 0.490566
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.0754716981132075): 0.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.18867924528302): 1.188679
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.943396226415094): 0.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.56603773584906): 2.566038
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.339622641509434): 0.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.358490566037736): 0.358491
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 4.11320754716981): 4.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.660377358490566): 0.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.49056603773585): 3.490566
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.830188679245283): 0.830189
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.11320754716981): 3.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.11320754716981): 1.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.56603773584906): 1.566038
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.24528301886792): 2.245283
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.32075471698113): 3.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.9622641509434): 2.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.09433962264151): 2.094340
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 6.05660377358491): 6.056604
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 16.0188679245283): 16.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 18.3396226415094): 18.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 39.4528301886792): 39.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.4905660377358): 44.490566
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 31.4905660377358): 31.490566
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 49.2641509433962): 49.264151
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 53.7735849056604): 53.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 63.4528301886792): 63.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 49.9622641509434): 49.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 47.0754716981132): 47.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 52.1509433962264): 52.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 39.3396226415094): 39.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.0188679245283): 44.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.1698113207547): 44.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 37.3584905660377): 37.358491
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 49.0377358490566): 49.037736
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.811320754717): 43.811321
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.377358490566): 44.377358
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 50.5094339622642): 50.509434
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 54.5094339622642): 54.509434
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 49.9245283018868): 49.924528
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 50.9811320754717): 50.981132
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 55.6792452830189): 55.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.3207547169811): 44.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 52.2641509433962): 52.264151
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 69.5471698113208): 69.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 57.8490566037736): 57.849057
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 56.1509433962264): 56.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 73.377358490566): 73.377358
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 68.2075471698113): 68.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 129.433962264151): 129.433962
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 157.528301886792): 157.528302
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 171.150943396226): 171.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 155.396226415094): 155.396226
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 177.301886792453): 177.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 206.169811320755): 206.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 195.924528301887): 195.924528
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 179.566037735849): 179.566038
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 183.396226415094): 183.396226
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 167.018867924528): 167.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 143.452830188679): 143.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 124.037735849057): 124.037736
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 109.11320754717): 109.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 108.11320754717): 108.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 103.716981132075): 103.716981
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 95.9622641509434): 95.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 66.2075471698113): 66.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 45.2264150943396): 45.226415
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 24.7924528301887): 24.792453
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 38.7547169811321): 38.754717
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 34.9811320754717): 34.981132
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 21.0566037735849): 21.056604
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 40.5660377358491): 40.566038
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 26.9811320754717): 26.981132
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 42.4150943396226): 42.415094
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 52.6603773584906): 52.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 38.9245283018868): 38.924528
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 50.7924528301887): 50.792453
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.2830188679245): 44.283019
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 37.4150943396226): 37.415094
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 34.6981132075472): 34.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 28.3396226415094): 28.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 25.0943396226415): 25.094340
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 31.9433962264151): 31.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 31.3584905660377): 31.358491
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 29.6792452830189): 29.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 21.3207547169811): 21.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 25.5471698113208): 25.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 28.377358490566): 28.377358
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 26.4716981132075): 26.471698
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 33.4339622641509): 33.433962
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 49.9811320754717): 49.981132
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 42.0377358490566): 42.037736
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.6037735849057): 44.603774
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 46.0377358490566): 46.037736
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 59.188679245283): 59.188679
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 63.8679245283019): 63.867925
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 87.6981132075472): 87.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 94.8490566037736): 94.849057
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 92.7735849056604): 92.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 63.3962264150943): 63.396226
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 50.1698113207547): 50.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 54.4716981132075): 54.471698
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 32.4150943396226): 32.415094
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 26.5283018867925): 26.528302
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 37.7358490566038): 37.735849
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 45.0566037735849): 45.056604
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 67.2830188679245): 67.283019
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 42.3396226415094): 42.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 39.8867924528302): 39.886792
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.2641509433962): 43.264151
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 40.9811320754717): 40.981132
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 46.2452830188679): 46.245283
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 56.4339622641509): 56.433962
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 42.7547169811321): 42.754717
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 25.1320754716981): 25.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 39.9622641509434): 39.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 53.5471698113208): 53.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 47.3207547169811): 47.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 60.811320754717): 60.811321
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 55.7547169811321): 55.754717
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 51.9622641509434): 51.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.5849056603774): 43.584906
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 48.6981132075472): 48.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 35.4716981132075): 35.471698
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 37.5471698113208): 37.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 41.8490566037736): 41.849057
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 27.5094339622642): 27.509434
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 17.1132075471698): 17.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 26.0754716981132): 26.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.622641509434): 43.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.7735849056604): 43.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 30.0188679245283): 30.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 36.0754716981132): 36.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 35.4905660377358): 35.490566
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 38.8490566037736): 38.849057
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 45.9622641509434): 45.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 47.7547169811321): 47.754717
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 48.1320754716981): 48.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 65.3207547169811): 65.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 82.9056603773585): 82.905660
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 98.6603773584906): 98.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 102.11320754717): 102.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 83.9622641509434): 83.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 62.1320754716981): 62.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 64.1320754716981): 64.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 74.5471698113208): 74.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 63.1698113207547): 63.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 56.9056603773585): 56.905660
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 59.7735849056604): 59.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.8679245283019): 43.867925
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 38.5660377358491): 38.566038
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.6603773584906): 44.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 45.4528301886792): 45.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 46.2075471698113): 46.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.6792452830189): 43.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 46.622641509434): 46.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 56.3018867924528): 56.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 50.7169811320755): 50.716981
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 61.2264150943396): 61.226415
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 72.7169811320755): 72.716981
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 78.9433962264151): 78.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 68.9433962264151): 68.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 59.6603773584906): 59.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 75.0943396226415): 75.094340
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 56.5094339622642): 56.509434
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 34.7735849056604): 34.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 37.4528301886792): 37.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 40.6792452830189): 40.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 58.0188679245283): 58.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 74.6981132075472): 74.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 85.3207547169811): 85.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 59.2641509433962): 59.264151
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 67.7735849056604): 67.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 77.6981132075472): 77.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 74.2452830188679): 74.245283
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 85.3396226415094): 85.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 99.4528301886792): 99.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 86.5849056603774): 86.584906
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 85.6037735849057): 85.603774
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 84.8679245283019): 84.867925
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 77.8301886792453): 77.830189
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 58.0377358490566): 58.037736
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 53.3584905660377): 53.358491
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 36.3207547169811): 36.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 20.7169811320755): 20.716981
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 27.3962264150943): 27.396226
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 40.0188679245283): 40.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 30.2075471698113): 30.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 25.5471698113208): 25.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 45.6603773584906): 45.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 33.5283018867925): 33.528302
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 19.622641509434): 19.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 19.0188679245283): 19.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 19.3396226415094): 19.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 33.3396226415094): 33.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 26.811320754717): 26.811321
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 21.1698113207547): 21.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 27.3018867924528): 27.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 21.3396226415094): 21.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 19.5471698113208): 19.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 21.3207547169811): 21.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 32.3018867924528): 32.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 20.1509433962264): 20.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 15.9433962264151): 15.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 17.2264150943396): 17.226415
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 23.4528301886792): 23.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 19.2452830188679): 19.245283
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 12.4528301886792): 12.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 8.0188679245283): 8.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 14.6603773584906): 14.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 16.3018867924528): 16.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 8.67924528301887): 8.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 7.79245283018868): 7.792453
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 8.13207547169811): 8.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.62264150943396): 2.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.45283018867925): 1.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.67924528301887): 3.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 4.81132075471698): 4.811321
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 8.50943396226415): 8.509434
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 7.07547169811321): 7.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 8.69811320754717): 8.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 9.75471698113208): 9.754717
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.20754716981132): 2.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.320754716981132): 0.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.113207547169811): 0.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.60377358490566): 1.603774
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 4.60377358490566): 4.603774
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.30188679245283): 3.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.84905660377358): 2.849057
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.830188679245283): 0.830189
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.962264150943396): 0.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.58490566037736): 1.584906
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.60377358490566): 2.603774
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 4.69811320754717): 4.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.30188679245283): 3.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.641509433962264): 0.641509
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.226415094339623): 0.226415
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.07547169811321): 1.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.71698113207547): 1.716981
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.339622641509434): 0.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.132075471698113): 0.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.150943396226415): 0.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.0754716981132075): 0.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.09433962264151): 2.094340
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.528301886792453): 0.528302
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.867924528301887): 0.867925
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.47169811320755): 1.471698
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.30188679245283): 0.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.132075471698113): 0.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.320754716981132): 0.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.679245283018868): 0.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.150943396226415): 0.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.339622641509434): 0.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.11320754716981): 1.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.83018867924528): 1.830189
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.169811320754717): 0.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')

## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.169811320754717): 0.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.377358490566038): 0.377358
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.264150943396226): 0.264151
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.13207547169811): 1.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.132075471698113): 0.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.226415094339623): 0.226415
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.54716981132075): 1.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.943396226415094): 0.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.207547169811321): 0.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.622641509433962): 0.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.62264150943396): 1.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.584905660377358): 0.584906
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.490566037735849): 0.490566
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.0754716981132075): 0.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.18867924528302): 1.188679
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.943396226415094): 0.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.56603773584906): 2.566038
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.339622641509434): 0.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.358490566037736): 0.358491
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 4.11320754716981): 4.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.660377358490566): 0.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.49056603773585): 3.490566
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.830188679245283): 0.830189
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.11320754716981): 3.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.11320754716981): 1.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.56603773584906): 1.566038
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.24528301886792): 2.245283
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.32075471698113): 3.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.9622641509434): 2.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.09433962264151): 2.094340
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 6.05660377358491): 6.056604
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 16.0188679245283): 16.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 18.3396226415094): 18.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 39.4528301886792): 39.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.4905660377358): 44.490566
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 31.4905660377358): 31.490566
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 49.2641509433962): 49.264151
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 53.7735849056604): 53.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 63.4528301886792): 63.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 49.9622641509434): 49.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 47.0754716981132): 47.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 52.1509433962264): 52.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 39.3396226415094): 39.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.0188679245283): 44.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.1698113207547): 44.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 37.3584905660377): 37.358491
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 49.0377358490566): 49.037736
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.811320754717): 43.811321
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.377358490566): 44.377358
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 50.5094339622642): 50.509434
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 54.5094339622642): 54.509434
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 49.9245283018868): 49.924528
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 50.9811320754717): 50.981132
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 55.6792452830189): 55.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.3207547169811): 44.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 52.2641509433962): 52.264151
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 69.5471698113208): 69.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 57.8490566037736): 57.849057
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 56.1509433962264): 56.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 73.377358490566): 73.377358
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 68.2075471698113): 68.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 129.433962264151): 129.433962
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 157.528301886792): 157.528302
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 171.150943396226): 171.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 155.396226415094): 155.396226
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 177.301886792453): 177.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 206.169811320755): 206.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 195.924528301887): 195.924528
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 179.566037735849): 179.566038
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 183.396226415094): 183.396226
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 167.018867924528): 167.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 143.452830188679): 143.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 124.037735849057): 124.037736
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 109.11320754717): 109.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 108.11320754717): 108.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 103.716981132075): 103.716981
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 95.9622641509434): 95.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 66.2075471698113): 66.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 45.2264150943396): 45.226415
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 24.7924528301887): 24.792453
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 38.7547169811321): 38.754717
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 34.9811320754717): 34.981132
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 21.0566037735849): 21.056604
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 40.5660377358491): 40.566038
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 26.9811320754717): 26.981132
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 42.4150943396226): 42.415094
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 52.6603773584906): 52.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 38.9245283018868): 38.924528
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 50.7924528301887): 50.792453
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.2830188679245): 44.283019
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 37.4150943396226): 37.415094
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 34.6981132075472): 34.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 28.3396226415094): 28.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 25.0943396226415): 25.094340
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 31.9433962264151): 31.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 31.3584905660377): 31.358491
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 29.6792452830189): 29.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 21.3207547169811): 21.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 25.5471698113208): 25.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 28.377358490566): 28.377358
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 26.4716981132075): 26.471698
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 33.4339622641509): 33.433962
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 49.9811320754717): 49.981132
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 42.0377358490566): 42.037736
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.6037735849057): 44.603774
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 46.0377358490566): 46.037736
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 59.188679245283): 59.188679
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 63.8679245283019): 63.867925
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 87.6981132075472): 87.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 94.8490566037736): 94.849057
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 92.7735849056604): 92.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 63.3962264150943): 63.396226
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 50.1698113207547): 50.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 54.4716981132075): 54.471698
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 32.4150943396226): 32.415094
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 26.5283018867925): 26.528302
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 37.7358490566038): 37.735849
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 45.0566037735849): 45.056604
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 67.2830188679245): 67.283019
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 42.3396226415094): 42.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 39.8867924528302): 39.886792
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.2641509433962): 43.264151
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 40.9811320754717): 40.981132
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 46.2452830188679): 46.245283
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 56.4339622641509): 56.433962
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 42.7547169811321): 42.754717
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 25.1320754716981): 25.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 39.9622641509434): 39.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 53.5471698113208): 53.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 47.3207547169811): 47.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 60.811320754717): 60.811321
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 55.7547169811321): 55.754717
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 51.9622641509434): 51.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.5849056603774): 43.584906
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 48.6981132075472): 48.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 35.4716981132075): 35.471698
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 37.5471698113208): 37.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 41.8490566037736): 41.849057
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 27.5094339622642): 27.509434
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 17.1132075471698): 17.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 26.0754716981132): 26.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.622641509434): 43.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.7735849056604): 43.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 30.0188679245283): 30.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 36.0754716981132): 36.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 35.4905660377358): 35.490566
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 38.8490566037736): 38.849057
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 45.9622641509434): 45.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 47.7547169811321): 47.754717
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 48.1320754716981): 48.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 65.3207547169811): 65.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 82.9056603773585): 82.905660
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 98.6603773584906): 98.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 102.11320754717): 102.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 83.9622641509434): 83.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 62.1320754716981): 62.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 64.1320754716981): 64.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 74.5471698113208): 74.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 63.1698113207547): 63.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 56.9056603773585): 56.905660
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 59.7735849056604): 59.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.8679245283019): 43.867925
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 38.5660377358491): 38.566038
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 44.6603773584906): 44.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 45.4528301886792): 45.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 46.2075471698113): 46.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 43.6792452830189): 43.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 46.622641509434): 46.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 56.3018867924528): 56.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 50.7169811320755): 50.716981
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 61.2264150943396): 61.226415
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 72.7169811320755): 72.716981
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 78.9433962264151): 78.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 68.9433962264151): 68.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 59.6603773584906): 59.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 75.0943396226415): 75.094340
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 56.5094339622642): 56.509434
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 34.7735849056604): 34.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 37.4528301886792): 37.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 40.6792452830189): 40.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 58.0188679245283): 58.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 74.6981132075472): 74.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 85.3207547169811): 85.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 59.2641509433962): 59.264151
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 67.7735849056604): 67.773585
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 77.6981132075472): 77.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 74.2452830188679): 74.245283
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 85.3396226415094): 85.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 99.4528301886792): 99.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 86.5849056603774): 86.584906
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 85.6037735849057): 85.603774
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 84.8679245283019): 84.867925
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 77.8301886792453): 77.830189
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 58.0377358490566): 58.037736
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 53.3584905660377): 53.358491
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 36.3207547169811): 36.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 20.7169811320755): 20.716981
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 27.3962264150943): 27.396226
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 40.0188679245283): 40.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 30.2075471698113): 30.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 25.5471698113208): 25.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 45.6603773584906): 45.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 33.5283018867925): 33.528302
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 19.622641509434): 19.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 19.0188679245283): 19.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 19.3396226415094): 19.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 33.3396226415094): 33.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 26.811320754717): 26.811321
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 21.1698113207547): 21.169811
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 27.3018867924528): 27.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 21.3396226415094): 21.339623
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 19.5471698113208): 19.547170
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 21.3207547169811): 21.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 32.3018867924528): 32.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 20.1509433962264): 20.150943
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 15.9433962264151): 15.943396
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 17.2264150943396): 17.226415
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 23.4528301886792): 23.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 19.2452830188679): 19.245283
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 12.4528301886792): 12.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 8.0188679245283): 8.018868
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 14.6603773584906): 14.660377
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 16.3018867924528): 16.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 8.67924528301887): 8.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 7.79245283018868): 7.792453
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 8.13207547169811): 8.132075
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.62264150943396): 2.622642
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.45283018867925): 1.452830
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.67924528301887): 3.679245
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 4.81132075471698): 4.811321
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 8.50943396226415): 8.509434
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 7.07547169811321): 7.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 8.69811320754717): 8.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 9.75471698113208): 9.754717
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.20754716981132): 2.207547
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.320754716981132): 0.320755
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.113207547169811): 0.113208
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.60377358490566): 1.603774
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 4.60377358490566): 4.603774
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.30188679245283): 3.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.84905660377358): 2.849057
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.830188679245283): 0.830189
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.962264150943396): 0.962264
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.58490566037736): 1.584906
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 2.60377358490566): 2.603774
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 4.69811320754717): 4.698113
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 3.30188679245283): 3.301887
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.641509433962264): 0.641509
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 0.226415094339623): 0.226415
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```
## Warning in `[<-.data.table`(`*tmp*`, x, 1, value = 1.07547169811321): 1.075472
## (type 'double') at RHS position 1 truncated (precision lost) when assigning to
## type 'integer' (column 1 named 'steps')
```

```r
head(newdata)
```

```
##    steps       date interval
## 1:     1 2012-10-01        0
## 2:     0 2012-10-01        5
## 3:     0 2012-10-01       10
## 4:     0 2012-10-01       15
## 5:     0 2012-10-01       20
## 6:     2 2012-10-01       25
```

4. A histrogram of the total number of steps taken each day is made.

```r
total_steps_new <- aggregate(steps~date, newdata, sum)
library(ggplot2)
ggplot(total_steps, aes(x=steps))+
  geom_histogram(fill = "blue", binwidth = 1000)+
  labs(title = "Daily Steps", x="Steps" , y= "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

The mean and median of the total number of steps taken each day are calculated.


```r
library(dplyr)
summary_new <- summarise(total_steps_new, mean_steps = mean(total_steps_new$steps),
                     median_steps = median(total_steps_new$steps))
print(summary_new)
```

```
##   mean_steps median_steps
## 1   10749.77        10641
```





## Are there differences in activity patterns between weekdays and weekends?
A new factor variable with two levels - "weekday" and "weekend" are created, each indicating whether a given day is a weekday or a weekend.



```r
newdata <- data.table::fread(input= "activity.csv")
newdata[, date := as.POSIXct(date, format = "%Y-%m-%d")]
newdata[, `Day of Week`:= weekdays(x = date)]
newdata[grepl(pattern = "Monday|Tuesday|Wednesday|Thursday|Friday", x = `Day of Week`), "weekday or weekend"] <- "weekday"
newdata[grepl(pattern = "Saturday|Sunday", x = `Day of Week`), "weekday or weekend"] <- "weekend"
newdata[, `weekday or weekend` := as.factor(`weekday or weekend`)]
head(newdata, 10)
```

```
##     steps       date interval Day of Week weekday or weekend
##  1:    NA 2012-10-01        0      Monday            weekday
##  2:    NA 2012-10-01        5      Monday            weekday
##  3:    NA 2012-10-01       10      Monday            weekday
##  4:    NA 2012-10-01       15      Monday            weekday
##  5:    NA 2012-10-01       20      Monday            weekday
##  6:    NA 2012-10-01       25      Monday            weekday
##  7:    NA 2012-10-01       30      Monday            weekday
##  8:    NA 2012-10-01       35      Monday            weekday
##  9:    NA 2012-10-01       40      Monday            weekday
## 10:    NA 2012-10-01       45      Monday            weekday
```

A panel plot contacontaining a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis), is made.


```r
library(ggplot2)
newdata[is.na(steps), "steps"] <- newdata[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
Interval <- newdata[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval, `weekday or weekend`)] 
ggplot(Interval , aes(x = interval , y = steps, color=`weekday or weekend`)) + geom_line() + labs(title = "Average Daily Steps by Weektype", x = "Interval", y = "No. of Steps") + facet_wrap(~`weekday or weekend` , ncol = 1, nrow=2)
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
