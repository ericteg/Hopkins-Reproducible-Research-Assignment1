# PA1_template
Eric Tegenfeldt  
May 14, 2016  

This project performs an analysis of exercise data taken from one individual during a two month period,
recorded at 5-minute intervals.  It analyzes the steps taken during that time period and looks at overall mean and median values by day, by the various 5-minute time intervals across days, and it also does some comparisons
of steps on weekdays versus the weekends.  It also creates a modified version of the original dataset that replaces over 2000 missing step values with the average steps for the combination of interval time and day of week of the observation that has the missing value, and it uses this updated dataset for a couple of the final analyses included in this project. It was thought that since there could be significant differences between weekday and weekend observations, it would be best to replace the missing values with averages that correspond not only to the same time of day (interval) but also the same day of the week.

## Obtaining, Loading, and Preparing the data.   

###  1. Download the data 

```r
if (!file.exists('.repdata-data-activity.zip')) {
        fileUrl = 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
        download.file(fileUrl,'.repdata-data-activity.zip', mode = 'wb')
        unzip(".repdata-data-activity.zip", exdir = './')
}
```

###  2. Read the activity.csv dataset extracted from the zip file.


```r
steps_data <- read.csv("activity.csv", sep = ',', header = TRUE, colClasses = c("numeric","character","integer"))
```

###  3. Convert the "date" column into true date format


```r
steps_data$date <- as.POSIXct(steps_data$date, format = "%Y-%m-%d")
```

###  4. Display some of the data.  Note that the interval value is expressed as hhmm with leading zeroes suppressed.


```r
head(steps_data,15)
```

```
##    steps       date interval
## 1     NA 2012-10-01        0
## 2     NA 2012-10-01        5
## 3     NA 2012-10-01       10
## 4     NA 2012-10-01       15
## 5     NA 2012-10-01       20
## 6     NA 2012-10-01       25
## 7     NA 2012-10-01       30
## 8     NA 2012-10-01       35
## 9     NA 2012-10-01       40
## 10    NA 2012-10-01       45
## 11    NA 2012-10-01       50
## 12    NA 2012-10-01       55
## 13    NA 2012-10-01      100
## 14    NA 2012-10-01      105
## 15    NA 2012-10-01      110
```

## For the initial analysis, display a histogram of the daily steps taken and determine the mean and median daily steps

###  1. Calculate total daily steps in order to produce histogram and determine mean and median values.


```r
steps_sum <- aggregate(steps ~ date, steps_data, sum)
```

###  2. Create a histogram of the daily total steps


```r
hist(steps_sum$steps, col="green", main = "Histogram of total daily steps", 
     xlab = "Daily Total", ylab = "Frequency", breaks=seq(from=0, to=25000, by=1000))
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

###  3. Calculate and display the mean daily steps


```r
mean(steps_sum$steps)
```

```
## [1] 10766.19
```

###  4. Calculate and display the median daily steps


```r
median(steps_sum$steps)
```

```
## [1] 10765
```

## For the next analysis, create a time series plot of the mean number of steps and identify the interval with the highest mean.

###  1.  First, calculate the average number of steps per interval value


```r
steps_intervals <- aggregate(steps ~ interval, steps_data, mean)
```

###  2.  Plot the time series of the average steps per interval


```r
plot(steps_intervals, type = 'l')
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

### 3.  Determine and display the interval with the highest mean number of steps


```r
x <- which.max(steps_intervals$steps)
steps_intervals[x,]
```

```
##     interval    steps
## 104      835 206.1698
```

** The interval with the highest mean (206.1698 steps) is 835


## For the next analysis, work with the missing values and replace them with an appropriate value

###  1.  First, display the total number of observations with missing steps values.


```r
sum(is.na(steps_data$steps))
```

```
## [1] 2304
```

###  2.  Add a new column of the dataset with each date identified by its day of the week


```r
steps_data$weekday <- weekdays(steps_data$date) 
```

###  3.  Calculate the mean value of steps for each interval number by the day of the week


```r
mean_steps <- aggregate(steps ~ weekday + interval, steps_data, mean) 
```

###  4.  Merge the previous result with the original data on interval and weekday and rename new columns


```r
steps_complete <- merge(steps_data, mean_steps, by=c("interval","weekday"))
names(steps_complete) <- c("interval","weekday","steps", "date","int_wkday_avg")
```

###  5.  Identify the rows with missing values and replace with the interval and weekday averages


```r
nas <- is.na(steps_complete$steps)
steps_complete$steps[nas] <- steps_complete$int_wkday_avg[nas]
```

###  6.  Confirm that all of the missing values in the new dataset have been removed


```r
sum(is.na(steps_complete$steps))
```

```
## [1] 0
```

## For the next analysis, repeat the original histogram but using the new dataset with no missing values

###  1. Calculate total daily steps in order to produce histogram and determine mean and median values.


```r
steps_sum2 <- aggregate(steps ~ date, steps_complete, sum)
```

###  2. Create a histogram of the daily total steps


```r
hist(steps_sum2$steps, col="green", main = "Histogram of total daily steps", 
     xlab = "Daily Total", ylab = "Frequency", breaks=seq(from=0, to=25000, by=1000))
```

![](PA1_template_files/figure-html/unnamed-chunk-19-1.png)<!-- -->


## For the final analysis, construct a panel plot comparing the interval averages between weekdays and weekends

###  1. First create an additional column "typeday" set to either "weekday" or "weekend" as appropriate.  This uses the mutate function so we first load the dplyr library


```r
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
steps_complete <- mutate(steps_complete, typeday = ifelse(steps_complete$weekday=="Saturday" | steps_complete$weekday=="Sunday", "weekend", "weekday"))
steps_complete$typeday <- as.factor(steps_complete$typeday)
```

###  2. Construct a panel plot comparing the average steps by interval between weekdays and weekends.  Load the lattice library first.


```r
library(lattice)
steps_sum3 <- aggregate(steps ~ interval + typeday, steps_complete, mean)
xyplot(steps ~ interval | typeday, data=steps_sum3, layout=c(1,2), type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

