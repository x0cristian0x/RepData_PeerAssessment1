---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Download data and to use library


```r
library(ggplot2)
library(lubridate)
library(dplyr)
library(lattice)

if(!dir.exists("Project1_RR")){
        dir.create("Project1_RR")}

URL<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(URL,destfile = "Project1_RR/data.zip")

unzip("Project1_RR/data.zip",exdir = "Project1_RR/data")
```


## Loading and preprocessing the data

Show any code that is needed to

1. Load the data (i.e. read.csv())

2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
data<-read.csv("Project1_RR/data/activity.csv")
data<-transform(data,date=as.Date(data$date,"%Y-%m-%d"))
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


## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Make a histogram of the total number of steps taken each day

2. Calculate and report the mean and median total number of steps taken per day


```r
day_steps<-data %>% 
        group_by(date) %>% 
        summarise(step_day=sum(steps))

hist(day_steps$step_day, main = "Histogram of step for day", 
     ylab = "Frecuency",  xlab = "Date")
abline(v=mean(day_steps$step_day,na.rm = TRUE), col="red",lwd=2 )
legend("topright",legend = "Mean", pch="l",col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

the mean is 1.0766189\times 10^{4} and the median is 10765.


## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
interval_mean<-data %>% 
        group_by(interval) %>% 
        summarise(mean_step=mean(steps,na.rm = TRUE)) 
        
plot(x=interval_mean$interval, y=interval_mean$mean_step ,
     type = "l", main = " Mean interval", ylab = "mean step",xlab = "Interval")
points(x=835, y=206,col="red",cex=2)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
inter_5min <-interval_mean[ which ( 
         interval_mean$mean_step==max( interval_mean$mean_step)),]
```

the intervalo more high 835 with 206.1698113


## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

the sum of missing values is 2304


```r
data_with_mean <-data
data_with_mean$steps[is.na(data_with_mean$steps)] <-mean(
  data_with_mean$steps,na.rm = TRUE)

total_steps<-data_with_mean %>% 
  group_by(date) %>% 
  summarise(m=sum(steps))

hist(total_steps$m,main = "Total step for day", 
     ylab = "Frecuency",  xlab = "Step" )
abline(v=mean(day_steps$step_day,na.rm = TRUE), col="red",lwd=2 )
legend("topright",legend = "Mean", pch="l",col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using __simulated data__:


```r
data<-transform(data,date=as.Date(date,"%Y-%m-%d"))
data$wday<-wday(data$date)

        for (i in  1:length(data$wday)){
        if (data$wday[i]=="1"|data$wday[i]=="7")
                data$week[i]="weekend"
        else data$week[i]="weekday"
        }

data<-transform(data, week=factor(week))

data<-data %>% 
  group_by(week,interval) %>% 
  summarise(m= mean(steps, na.rm = TRUE))

xyplot(m~interval|week,data = data,aspect=1/2,type="l",main="Interval for weeks",ylab = "Week", xlab = "Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

© 2020 GitHub, Inc.
