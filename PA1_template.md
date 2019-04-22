---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

Personal activity monitoring
============================


### Downloading and unzipping the data
Lets see if its downloaded already, if not lets do so.

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.5.3
```

```r
destfile="./data.zip"
if(!file.exists(destfile)){
      url <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
      download.file(url, destfile = "./data.zip")
}
unzip("./data.zip")
```
###data processing

Now we read the data to a data frame. then we summarize the data with summary, its string representation and the header.

```r
data <- read.table(file = "activity.csv", header = TRUE, sep = ",", nrows =  17568, na.strings = NA)
summary(data)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

```r
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```
 
now we transform the date to the YYYY-MM-DD format

```r
data$date <- as.Date(data$date, "%Y-%m-%d")
class(data$date)
```

```
## [1] "Date"
```

```r
head (data$date)
```

```
## [1] "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01"
## [6] "2012-10-01"
```

## Assignment questions
### 1. What is mean total number of steps taken per day?

1. calculate the number of steps per day

```r
steps_per_day <- with(data, tapply(steps, date, sum, na.rm = TRUE))
str (steps_per_day)
```

```
##  int [1:61(1d)] 0 126 11352 12116 13294 15420 11015 0 12811 9900 ...
##  - attr(*, "dimnames")=List of 1
##   ..$ : chr [1:61] "2012-10-01" "2012-10-02" "2012-10-03" "2012-10-04" ...
```

```r
head (steps_per_day)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##          0        126      11352      12116      13294      15420
```

Now we make a histogram and print out the mean and median of the total number of steps taken each day.

```r
hist(steps_per_day, xlab = "Total steps per day", col = "red", main = "Total steps taken each day (ignore missing data)")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
mean_steps_per_day<-mean(steps_per_day)
median_steps_per_day<-median(steps_per_day)
paste(mean_steps_per_day, " mean steps per day")
```

```
## [1] "9354.22950819672  mean steps per day"
```

```r
paste(median_steps_per_day, "median steps per day")
```

```
## [1] "10395 median steps per day"
```
### 2. What is the average daily activity pattern?

First, we need to calculate the average number of steps taken per each 5-minutes interval (averaged across all days). Then we make a plot of intervals and the average numbers of steps taken.

```r
steps_per_interval =  with(data, tapply(steps, interval, mean, na.rm = TRUE))
plot(unique(data$interval), steps_per_interval, type = "l", xlab = "5-minute interval", ylab = "steps per interval averaged over all days", main = "Average daily activity")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

The 5 minute interval which on average contains the most steps

```r
names (which(steps_per_interval==max(steps_per_interval)))
```

```
## [1] "835"
```

### 3. Inputing missing values 
1. report the number of NAs

```r
sum(is.na(data))
```

```
## [1] 2304
```

Well change the missing values with the mean of the number of steps for that 5 minute interval, and place it into a copy of the original data called data2

```r
data2 <- data
data2$steps_per_interval =  with(data2, tapply(steps, interval, mean, na.rm = TRUE))
data2[is.na(data2$steps),]$steps <- data2[is.na(data2$steps),]$steps_per_interval
head(data2)
```

```
##       steps       date interval steps_per_interval
## 1 1.7169811 2012-10-01        0          1.7169811
## 2 0.3396226 2012-10-01        5          0.3396226
## 3 0.1320755 2012-10-01       10          0.1320755
## 4 0.1509434 2012-10-01       15          0.1509434
## 5 0.0754717 2012-10-01       20          0.0754717
## 6 2.0943396 2012-10-01       25          2.0943396
```
We will now redraw the histogram to see how changing the NAs into mean values and see how it impacts the graph.


```r
steps_per_day_2 <- with(data2, tapply(steps, date, sum, na.rm = T))
hist(steps_per_day_2, xlab = "Total number of steps per day", col = "wheat2", main = "Total number of steps taken each day (with filled missing data)")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
mean(steps_per_day_2)
```

```
## [1] 10766.19
```

```r
median(steps_per_day_2)
```

```
## [1] 10766.19
```
we can see the histogram has been skewed towards the mean of the number of steps now, instead of being skewed to zero steps

### 4. Are there differences in activity patterns between business days and weekends?

Add a new "weekday" column:


```r
data2$weekday = weekdays(strptime(data2$date,"%Y-%m-%d"))
```

subset the data2 data frame into weekend and weekday


```r
data_weekdays <- subset(subset(data2,weekday != 'Saturday'),weekday != 'Sunday')
data_weekends <- rbind(subset(data2,weekday == 'Saturday'),subset(data2,weekday == 'Sunday'))
```

Make a panel plot:


```r
par(mfrow=c(2,1))
with(aggregate(steps ~ interval, data_weekends, mean),plot(interval,steps,type="l",main="Weekend"))
with(aggregate(steps ~ interval, data_weekdays, mean),plot(interval,steps,type="l",main="Weekday"))
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
