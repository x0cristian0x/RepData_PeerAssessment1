---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
## Loading and preprocessing the data


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

```r
day_steps<-data %>% 
        group_by(date) %>% 
        summarise(step_day=sum(steps))

hist(day_steps$step_day, main = "Histogram of step for day", 
     ylab = "Frecuency",  xlab = "Date")
```

![](Project_1_RR_m_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

the mean is 1.0766189\times 10^{4} and the median is 10765.



## What is the average daily activity pattern?


```r
interval_mean<-data %>% 
        group_by(interval) %>% 
        summarise(mean_step=mean(steps,na.rm = TRUE)) 
        
plot(x=interval_mean$interval, y=interval_mean$mean_step ,
     type = "l", main = " Mean interval", ylab = "mean step",xlab = "Interval")
```

![](Project_1_RR_m_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
inter_5min <-interval_mean[ which ( 
         interval_mean$mean_step==max( interval_mean$mean_step)),]
```

the intervalo more high 835 with 206.1698113


## Imputing missing values
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
```

![](Project_1_RR_m_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


## Are there differences in activity patterns between weekdays and weekends?

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

![](Project_1_RR_m_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

© 2020 GitHub, Inc.
