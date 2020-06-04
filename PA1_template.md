Reproducible Research Assignment 1
================
Patricia Londono
June 2, 2020

## Introduction

It is now possible to collect a large amount of data about personal
movement using activity monitoring devices such as a Fitbit, Nike
Fuelband, or Jawbone Up. These type of devices are part of the
“quantified self” movement - a group of enthusiasts who take
measurements about themselves regularly to improve their health, to find
patterns in their behavior, or because they are tech geeks. But these
data remain under-utilized both because the raw data are hard to obtain
and there is a lack of statistical methods and software for processing
and interpreting the data.

This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012 and
include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

Dataset: Activity monitoring data

## Reading the data

``` r
# Downloading the data
if(!file.exists('data.zip')){
  url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file(url,destfile = "data.zip")
}
unzip("data.zip")

data <- read.csv("activity.csv") 
```

## Exploring the Dataset

The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are
coded as NA)

date: The date on which the measurement was taken in YYYY-MM-DD format

interval: Identifier for the 5-minute interval in which measurement was
taken

``` r
str(data)
```

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

## 1\. What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in
the dataset.

#### Make a histogram of the total number of steps taken each day

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------------------------------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.1     v purrr   0.3.4
    ## v tibble  3.0.1     v dplyr   1.0.0
    ## v tidyr   1.1.0     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts ------------------------------------------------------------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
dataAgg <- aggregate(steps ~ date, data, sum)

ggplot(dataAgg, aes(factor(date), steps)) +
  geom_bar(stat="identity", fill='red') +
  theme_bw() + guides(fill=FALSE)+
  labs(x="Date", y=expression("Total Number of Steps")) + 
  labs(title=expression("Total Number of Steps a Day")) +
  theme(text = element_text(size=10),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle=90, hjust=0.5))
```

![](PA1_template_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

#### Calculate and report the mean and median total number of steps taken per day

``` r
meanSteps <- mean(dataAgg$steps, na.rm = TRUE)
medianSteps <- median(dataAgg$steps, na.rm = TRUE)
```

Mean total number of steps: 1.076618910^{4}

Median total number of steps: 10765

## What is the average daily activity pattern?

#### 1.Make a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

``` r
intervalAgg <- aggregate(steps ~ interval, data, mean)
ggplot(data = intervalAgg, aes(x = interval, y = steps)) +
  geom_line() +
  ggtitle("Total Number of Steps by Intervals") +
  xlab("5-minute Interval") +
  ylab("Number of Steps") +
  theme(plot.title = element_text(hjust = 0.5))
```

![](PA1_template_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

#### 2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps

``` r
max_Interval <- intervalAgg[which.max(intervalAgg$steps),1:2]
```

Maximun number of Steps: 206.1698113 Time: 835

## Imputing missing values

Note that there are a number of days/intervals where there are missing
values (coded as NA). The presence of missing days may introduce bias
into some calculations or summaries of the data.

#### 1\. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

``` r
NAs <- length(which(is.na(data$steps)))
```

Total Number of missing values: 2304

#### 2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

``` r
NAs <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
data2 <- data
data2[] <- lapply(data, NAs)
```

    ## Warning in mean.default(x, na.rm = TRUE): argument is not numeric or logical:
    ## returning NA

#### 3.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

``` r
dataAgg2 <- aggregate(steps ~ date, data2, sum)

ggplot(dataAgg2, aes(factor(date), steps)) +
  geom_bar(stat="identity", fill='blue') +
  theme_bw() + guides(fill=FALSE)+
  labs(x="Date", y=expression("Total Number of Steps")) + 
  labs(title=expression("Total Number of Steps a Day")) +
  theme(text = element_text(size=10),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle=90, hjust=0.5))
```

![](PA1_template_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
meanSteps2 <- mean(dataAgg2$steps, na.rm = TRUE)
medianSteps2 <- median(dataAgg2$steps, na.rm = TRUE)
```

Mean total number of steps: 1.076618910^{4}

Median total number of steps: 1.076618910^{4}

``` r
diffmeanSteps <- meanSteps2 - meanSteps
diffmedianSteps <- medianSteps2 - medianSteps
```

Difference in mean number of steps: 0 This number did not change as
missing values were replaced with the mean

Difference in mean number of steps: 1.1886792

## Are there differences in activity patterns between weekdays and weekends?

#### 1\. Create a new factor variable in the dataset with two levels - “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
data2$date <- ymd(data2$date)
data2$day <- ifelse(weekdays(data2$date) %in% c("Saturday","Sunday"), "Weekday", "Weekend")
```

#### 2\. Make a panel plot containing a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

``` r
library(lattice)

weekdaysSummary <- aggregate(steps ~ interval + day, data = data2, mean)

xyplot(steps ~ interval | day, data = weekdaysSummary, type = "l", lwd = 2,
       layout = c(1, 2), 
       xlab = "5-minute interval", 
       ylab = "Mean number of steps",
       main = "Mean Number of Steps by Weekday")
```

![](PA1_template_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->
