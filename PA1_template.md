---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: yes
---
# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data

##### 1. Load the data (i.e. read.csv())


```r
 if(!file.exists('activity.csv')){
  
   unzip('activity.zip')
  }

activity <- read.csv('activity.csv')
```

  
## What is mean total number of steps taken per day?


```r
MTsteps <- tapply(activity$steps, activity$date, sum)
```

##### 1. Make a histogram of the total number of steps taken each day


```r
hist(MTsteps,main = "Total Steps per Day",xlab = "Number of Steps",col = rgb(1,0.2,0.6))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

##### 2. Calculate and report the mean and median of the total number of steps taken per day


```r
Mean <- mean(MTsteps,na.rm = TRUE)
Median <- median(MTsteps,na.rm = TRUE)
```
* Mean: 1.0766189\times 10^{4}
* Median:  10765

-----
  
## What is the average daily activity pattern?
  

```r
adap <- aggregate(steps ~ interval, activity, mean)
```

##### 1. Make a time series plot


```r
ggplot(data = adap, aes(x = interval, y = steps)) + geom_line(colour = rgb(1,0.4,0.4)) +labs(x="5-minute Interval", y ="Average Number of Steps",title = "Average Daily Activity Pattern" )
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

##### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
Max <- adap[adap$steps== max(adap$steps),1]
```

* The 5-minute interval is : 830 - 835

----
  
## Imputing missing values
  
##### 1. Calculate and report the total number of missing values in the dataset 


```r
NMvalues <- sum(is.na(activity$steps))
```

* Number of missing values: 2304

##### 2. Devise a strategy for filling in all of the missing values in the dataset.

##### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
FDactivity <- activity

FDactivity$steps <- impute(activity$steps, fun=mean) # here I feel all the missing values by the mean value of the steps variable of activity
```


##### 4. Make a histogram of the total number of steps taken each day 


```r
MTsteps2 <- tapply(FDactivity$steps, FDactivity$date, sum)

hist(MTsteps2,main = "Imputed total Steps per Day",xlab = "Number of Steps",col = rgb(0,1,1))
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

##### ... and Calculate and report the mean and median total number of steps taken per day. 

```r
IMean <- mean(MTsteps2)

IMedian <- median(MTsteps2)
```
* Mean (Imputed): 1.0766189\times 10^{4}
* Median (Imputed):  1.0766189\times 10^{4}


----
  
## Are there differences in activity patterns between weekdays and weekends?
  
##### 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
  

```r
FDactivity$date <- as.Date(FDactivity$date,format = "%Y-%m-%d")
dummy <- weekdays(FDactivity$date)
dummy <- sub("Friday|Monday|Thursday|Tuesday|Wednesday","weekday",dummy)
dummy <- sub("Saturday|Sunday","weekend",dummy)
dummy <- as.factor(dummy)
FDactivity$day_type <- dummy
```

##### 2. Make a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken across all weekdays or weekends


```r
ASPI <- aggregate(steps ~ interval + day_type, FDactivity, mean)

ggplot(data = ASPI, aes(x = interval, y = steps)) + geom_line() + facet_grid(day_type ~ .) +labs(x="5-minute Interval", y ="Average Number of Steps",title = "Average Daily Activity Pattern" )
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
