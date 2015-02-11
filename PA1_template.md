---
title: "Activity Monitoring (Peer Assessment 1)"
author: "Ke Li"
date: "02/10/2015"
output: pdf_document
---
Activity monitoring devices measuring human movements regularly is aiming to find patterns in human behavior, and improve human health. 
Data is downloaded from [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) to explore insight of monintered steps every 5 minuntes daily from October 1st to November 30th 2012.

## Loading and preprocessing the data

The data can be downloaded from [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip), and saved as 'activity.zip'


```r
url = 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
download.file(url, 'activity.zip', method='wget')
```
Decompress the zip data, using the 'unzip()' command in R:

```r
unzip('./activity.zip')
```
After unzipping, find the file 'activity.csv':

```r
list.files()
```

```
##  [1] "Abcd1234"          "Abcd1234.pub"      "activity.csv"     
##  [4] "activity.zip"      "doc"               "figures"          
##  [7] "instructions_fig"  "missfont.log"      "PA1_template.html"
## [10] "PA1_template.Rmd"  "q.log"             "README.md"
```
Read in 'activity.csv', using 'read.csv()' in R. Name the read-in data 'activity':

```r
activity = read.csv('./activity.csv',
                    colClasses = c('numeric', 'factor', 'numeric'))
```
Explore the structure of the data, using 'str()', 'head()', 'summary()' functions in R:

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: num  0 5 10 15 20 25 30 35 40 45 ...
```


```r
head(activity)
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

```r
summary(activity)
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

Therefore, data 'activity' contains three variables:

* steps

* date

* interval

with 17568 records, where 2304 records have 'NA' as steps value; date is not recognized as Date variable in R. Therefore, convert the date column from Factor format into Date format using 'as.Date()' function. Once conversion is done, delete the orignal date column to avoid confusion.

```r
activity$dateformat = as.Date(activity$date, format = '%Y-%m-%d')
activity$date = NULL
```
## What is mean total number of steps taken per day?
1. The histogram of the total number of steps taken each day is plotted as following:

```r
par(mfrow = c(1, 1), mar = c(4, 4, 1 , 1), oma = c(1, 1, 0, 0))
hist(activity$steps, ylim = c(0, 15000))
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

```r
# to save it:
#dev.copy(png, file ='histogram.png')
#dev.off()
```
2. The mean and median total number of steps taken per day are calculated as:

```r
mean(activity$steps, na.rm=T)
```

```
## [1] 37.3826
```

```r
median(activity$steps, na.rm=T)
```

```
## [1] 0
```
##  What is the average daily activity pattern
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis):

```r
#intervalMean is the mean of the steps at each 5 minute interval over all the days
intervalMean = with(activity, tapply(steps, interval,  mean, na.rm=T))
#intervalDaily is the minutes number of the intervals. 
# Everyday appears the same pattern in munites (ex, 0, 5, 10, .....2355)
intervalDaily = unique(activity$interval)
# plot time series
plot(intervalDaily, intervalMean, type='l', main = 'Average steps and daily interval',
     xlab='5-minute interval', ylab='average steps')
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

```r
# to save the plot into file:
#dev.copy(png, file ='5minAve.png')
#dev.off()
```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
intervalDaily[which.max(intervalMean)]
```

```
## [1] 835
```
## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
table(is.na(activity$steps))
```

```
## 
## FALSE  TRUE 
## 15264  2304
```

2. To fill the NAs in the data 'activity', strategy is developed to replace NAs with the average steps at the minute interval when the step is recorded over the days. To develop this strategy, the data 'activity' is duplicated to a data table called 'DT'.

```r
library(data.table)
```

```
## data.table 1.9.4  For help type: ?data.table
## *** NB: by=.EACHI is now explicit. See README to restore previous behaviour.
```

```r
DT = data.table(activity)
#In DT, a new variable 'intervalMean' is set to take the mean of the subsets of records, categorized by interval values
DT[, intervalMean := mean(steps, na.rm=T), by=interval]
```

```
##        steps interval dateformat intervalMean
##     1:    NA        0 2012-10-01    1.7169811
##     2:    NA        5 2012-10-01    0.3396226
##     3:    NA       10 2012-10-01    0.1320755
##     4:    NA       15 2012-10-01    0.1509434
##     5:    NA       20 2012-10-01    0.0754717
##    ---                                       
## 17564:    NA     2335 2012-11-30    4.6981132
## 17565:    NA     2340 2012-11-30    3.3018868
## 17566:    NA     2345 2012-11-30    0.6415094
## 17567:    NA     2350 2012-11-30    0.2264151
## 17568:    NA     2355 2012-11-30    1.0754717
```
3. Fill in the NAs in the new data set (DT):

```r
DT[is.na(steps),]$steps = DT[is.na(steps),]$intervalMean
```

4. With all the NAs filled in step variable (DT), the mean and median are calculated to compare with the steps in raw data activity with NAs (activity). The means and medians before and after filling NAs with means are the same.

```r
mean(DT$steps, na.rm=T)
```

```
## [1] 37.3826
```

```r
mean(activity$steps, na.rm=T)
```

```
## [1] 37.3826
```

```r
median(DT$steps, na.rm=T)
```

```
## [1] 0
```

```r
median(activity$steps, na.rm=T)
```

```
## [1] 0
```
Two histograms of steps in DT and activity are plotted to compare. Both histograms before and after filling NAs with means are almost the same. 

```r
par(mfrow = c(2, 1), mar = c(2, 2, 1 , 1), oma = c(1, 1, 0, 0))
hist(DT$steps, ylim=c(0, 15000), breaks = 40, main ='Histogram of Filled-in Data')
#abline(v = mean(DT$steps, na.rm=T), col = 'red')
hist(activity$steps, ylim = c(0, 15000), breaks=40, main = 'Histogram of raw Data')
#abline(v = mean(activity$steps, na.rm=T), col = 'red')
mtext('Steps', out=T, side=1)
mtext('Frequency', out=T, side=2)
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17-1.png) 

```r
# to save this plot:
#dev.copy(png, file ='histogram_filled.png')
#dev.off()
```
## Are there differences in activity patterns between weekdays and weekends?
1. A new factor variable in the dataset is created with two levels -"weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
# convert formatted date as weekdays using weekdays()
activity$daytype[(weekdays(activity$dateformat) == 'Saturday')
                 |(weekdays(activity$dateformat) == 'Sunday')] = 'Weekend'
activity$daytype[!((weekdays(activity$dateformat) == 'Saturday')
                 |(weekdays(activity$dateformat) == 'Sunday'))] = 'Weekday'
# convert the daytype to factor from character
activity$daytype = as.factor(activity$daytype)
```
2. A panel plot is made containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis):

```r
meanDaily = with(activity, tapply(steps, list(interval, daytype), mean, na.rm=T))
par(mfrow = c(2, 1), mar = c(2, 2, 1 , 1), oma = c(1, 1, 0, 0))
plot(intervalDaily, meanDaily[, 1], type='l', main = 'Weekend', xlab= '', ylim = c(0, 250))
plot(intervalDaily, meanDaily[, 2], type='l', main = 'Weekday', xlab='interval',
     ylim = c(0, 250))
mtext('Number of steps', outer=T, side=2)
mtext('Interval', outer=T, side=1)
```

![plot of chunk unnamed-chunk-19](figure/unnamed-chunk-19-1.png) 

```r
#dev.copy(png, file ='weekdays.png')
#dev.off()
```
It appears that there is a difference between weekdays and weekends regarding the steps taken. During the weekends, average number of steps peaks around 9:00am. During the weekdays, the average steps are relatively more evenly distributed along the day time period. 
