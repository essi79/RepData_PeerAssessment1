# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
###first we will unzip the data file that is included in the repo


```r
zipfile<-"activity.zip"
unzip(zipfile)
```

###we now need to read in the dataset using read.csv, since it is in csv format

```r
activityData<- read.csv("activity.csv",stringsAsFactors = FALSE)
```
###Lets take a look at the class of variables in each column

```r
str(activityData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

###it appears that the dates are in character format. We need to conver them to dates


```r
activityData$date<-as.Date(activityData$date)
```

## What is mean total number of steps taken per day?

###First, we calculate the total number of steps taken per day

```r
stepsPerDay<-with(activityData, tapply(steps, date, sum))
```
###then we histogram the total number of steps taken per day

```r
hist(stepsPerDay)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
###And finaly we are ready to report mean and median values

```r
mean(stepsPerDay,na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(stepsPerDay,na.rm = TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

###first we calculate the average steps in each interval after removing NA values

```r
averageStepsPerInterval<-with(activityData[!is.na(activityData$steps),], tapply(steps, interval, mean))
names(averageStepsPerInterval)<-activityData$interval[1:288]
```
###then we plot the average number of steps against interval count

```r
plot(averageStepsPerInterval,type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->
###finally we show which interval has the highest average steps. 

```r
averageStepsPerInterval[which.max(averageStepsPerInterval)]
```

```
##      835 
## 206.1698
```

## Imputing missing values
###Calculating the total number of missing values in the dataset

```r
sum(is.na(activityData$steps))
```

```
## [1] 2304
```

###we now devise a strategy to fill in the missing values. Our strategy is that we replace the missing value by using the average number of steps in the same interval from the previous calculation(of the intervals that have values)

```r
reppedAverageStepsPerInterval<-rep(averageStepsPerInterval,nrow(activityData)/length(averageStepsPerInterval))
NoNAActivityData<-activityData
NoNAActivityData$steps[is.na(activityData$steps)]<-reppedAverageStepsPerInterval[is.na(activityData$steps)]
```

###We have created a new dataset that has the missing values filled in. We now display the histogram, and the mean and median values. Because of the strategy employed, it seems that the missing values do not have a tangible impact on the mean and median values. 


```r
NoNAstepsPerDay<-with(NoNAActivityData, tapply(steps, date, sum))
hist(NoNAstepsPerDay)
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

```r
mean(NoNAstepsPerDay,na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(NoNAstepsPerDay,na.rm = TRUE)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?
###Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
library("dplyr")
```

```
## Warning: package 'dplyr' was built under R version 3.3.2
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
dayVector<-c("Weekday","Weekday","Weekday","Weekday","Weekday","Weekend","Weekend")
names(dayVector)<-c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")

NoNAActivityData <- mutate(NoNAActivityData, dayType=as.factor(dayVector[weekdays(date)]))
```


###Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
weekdayAverageStepsPerInterval<-with(NoNAActivityData[NoNAActivityData$dayType=="Weekday",], tapply(steps, interval, mean))
weekendAverageStepsPerInterval<-with(NoNAActivityData[NoNAActivityData$dayType=="Weekend",], tapply(steps, interval, mean))
par(mfrow=c(2,1),mar=c(2,3,1,1))
plot(weekdayAverageStepsPerInterval,type="l",main="Weekday Average steps per interval")
plot(weekdayAverageStepsPerInterval,type="l",main="Weekend Average steps per interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->
