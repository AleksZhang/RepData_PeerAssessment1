# Course Project 1
Aleks Zhang  
5 октября 2016 г.  

#Reproducible Research

##Course Project 1

###1. Loading and preprocessing the data

First of all, we're loading packages which we'll need for the analysis and data.


```r
library(knitr)
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
library(lattice)

activity <- as.data.frame(read.csv("activity.csv"))
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

###2. What is mean total number of steps taken per day?

We calculate total number of steps per day.


```r
t01 <- activity %>% 
  group_by(date) %>%
  summarise(sumsteps = sum(steps))
```

Then, we make a histogram.


```r
hist(t01$sumsteps, col="green", main="Steps per day", xlab="Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

After this, we calculate mean and median number of steps per day.


```r
mean <- mean(t01$sumsteps, na.rm=T)
median <- median(t01$sumsteps, na.rm=T)
```

So, mean is **1.0766189\times 10^{4}** 
and median is **10765** 

###3. What is the average daily activity pattern?

We're plotting the graph for daily acticity.


```r
t02 <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)
plot(t02, type="l", xlab = "intervals", ylab = "avg. steps number")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
t02[t02==max(t02)]
```

```
##      835 
## 206.1698
```


###4. Imputing missing values

We find out what is the number of NAs.


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

Then we replace NAs with average steps number for each particular interval.


```r
activity2 <- activity %>% mutate(steps = ifelse(is.na(steps), 
      t02, 
      steps))
```

After this, we're plotting for the histogram:


```r
t03 <- activity2 %>% 
  group_by(date) %>%
  summarise(sumsteps = sum(steps))

hist(t03$sumsteps, col="green", main="Steps per day", xlab="Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

Counting mean and median:

```r
mean2 <- mean(t03$sumsteps)
median2 <- median(t03$sumsteps)
```

So, mean is **1.0766189\times 10^{4}** 
and median is **1.0766189\times 10^{4}** 

Imputting missing data made median equal to mean.

###5. Are there differences in activity patterns between weekdays and weekends?

We create variable for weekdays:


```r
activity2$date <- as.Date(as.character(activity2$date))

activity3 <- activity2 %>% mutate(date = as.Date(as.character(date))) %>%
                                 mutate(weekday = as.factor(ifelse(
                                   as.numeric(as.factor(weekdays(activity2$date))) %in% c(6,7),
                                      "weekend",
                                      "weekday")))
```

and plot for the differences between weekdays and weekends.


```r
activity4<- activity3 %>% 
     group_by(weekday,interval) %>%
     summarise(meansteps=mean(steps))
with (activity4, 
             xyplot(meansteps ~ interval|weekday, type="l", 
                                   ylab="Number of steps",layout=c(1,2)))      
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

Daily activity on weekdays and weekends has differences.
