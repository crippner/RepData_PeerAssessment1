---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
library(tidyverse)
```

```
## -- Attaching packages ------------------------------------------------------- tidyverse 1.3.0 --
```

```
## v ggplot2 3.3.2     v purrr   0.3.4
## v tibble  3.0.3     v dplyr   1.0.0
## v tidyr   1.1.0     v stringr 1.4.0
## v readr   1.3.1     v forcats 0.5.0
```

```
## -- Conflicts ---------------------------------------------------------- tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
stepdata_raw <- read_csv("activity.csv",
                trim_ws = TRUE,
                col_names = TRUE)
```

```
## Parsed with column specification:
## cols(
##   steps = col_double(),
##   date = col_date(format = ""),
##   interval = col_double()
## )
```

```r
stepdata <- stepdata_raw
stepdata$steps[is.na(stepdata$steps)] <- 0
  
stepdata1 <- stepdata %>%
  group_by(date) %>%
  summarise(total = sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
stepdata2 <- stepdata %>%
  group_by(interval) %>%
  summarise(avgint = mean(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

## What is mean total number of steps taken per day?

```r
ggplot(stepdata1, 
       mapping = aes(x = total), na.rm = TRUE) +
        geom_histogram() +
        labs(x = "Total daily steps",
             title = "Histogram, total daily steps")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/Mean Total Steps per Day-1.png)<!-- -->

```r
meansteps <- mean(stepdata1$total)
mediansteps <- median(stepdata1$total)
```
### The mean steps taken per day is 9354.2295082. The median steps taken per day is 1.0395\times 10^{4}.

## What is the average daily activity pattern?

```r
ggplot(stepdata2, mapping = aes(x = interval, y = avgint)) +
        geom_line(size = 1, color = "steelblue4") +
        labs(x = "Interval",
             y = "Average Steps, all days",
             title = "Average daily activity pattern")
```

![](PA1_template_files/figure-html/Daily Activity Pattern-1.png)<!-- -->

```r
maxint <- stepdata2$interval[which.max(stepdata2$avgint)]
```
### The interval containing the maximum number of steps (averaged across all days) is 835.

## Imputing missing values

```r
missingdata <- sum(!complete.cases(stepdata_raw))
```
### The total number of missing values in the data set is 2304.

```r
# Stategy: make a list from original steps where all NA's replaced by avg steps
# for that interval, as calculated in the previous code chunk.

rawsteps <- stepdata_raw$steps
avgsteps <- stepdata2$avgint

impsteps <- replace(rawsteps, which(is.na(rawsteps)), avgsteps)

# New dataset with imputed steps
stepdata3 <- data.frame(impsteps, stepdata_raw$date, stepdata_raw$interval)
names(stepdata3)<- c("impsteps", "date", "interval")
```


```r
# calculate daily totals now with imputed step data
stepdata4 <- stepdata3 %>%
  group_by(date) %>%
  summarise(total = sum(impsteps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
# Histogram
ggplot(stepdata4, 
       mapping = aes(x = total), na.rm = TRUE) +
        geom_histogram() +
        labs(x = "Total daily steps with imputed data",
             title = "Histogram, total daily steps with imputed data")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/Characterizing the new dataset with imputed data-1.png)<!-- -->

```r
meansteps.2 <- mean(stepdata4$total)
mediansteps.2 <- median(stepdata4$total)
```
### The mean steps taken per day is 1.0581014\times 10^{4}. The median steps taken per day is 1.0395\times 10^{4}.
### The new mean is greater than before, but the median remains the same.
### The total daily number of steps is no longer skew toward 0, but now toward the median.

## Are there differences in activity patterns between weekdays and weekends?

```r
weekend.list <- c("Saturday", "Sunday")

stepdata5 <- mutate(stepdata3, day = weekdays(date))

day.list <- data.frame(day.type = NULL)
# for the weekend list, if match is na, then store "weekday"
# for the weekend list, if match is not na, then store "weekend"

m <- match(stepdata5$day, weekend.list)

day.list <- if_else(is.na(m), "weekday", "weekend")

stepdata5 <- mutate(stepdata5, day.list)

# indices for weekdays
i1 <- which(!is.na(match(stepdata5$day.list, "weekday")))

# indices for weekends
i2 <- which(!is.na(match(stepdata5$day.list, "weekend")))

# average steps over all weekdays
stepdata5.weekday.ave <- stepdata5[i1,] %>%
  group_by(interval, day.list) %>%
  summarise(meansteps = mean(impsteps))
```

```
## `summarise()` regrouping output by 'interval' (override with `.groups` argument)
```

```r
# average steps over all weekend days
stepdata5.weekend.ave <- stepdata5[i2,] %>%
  group_by(interval, day.list) %>%
  summarise(meansteps = mean(impsteps))
```

```
## `summarise()` regrouping output by 'interval' (override with `.groups` argument)
```

```r
stepdata5.interval.ave <- rbind(
  stepdata5.weekday.ave,
  stepdata5.weekend.ave)

# Panel (facet) plot
ggplot(stepdata5.interval.ave, mapping = aes(x = interval , y = meansteps)) +
        geom_line(size = 1, color = "orangered2") +
        facet_grid(rows = vars(day.list))
```

![](PA1_template_files/figure-html/Weekend and Weekday patterns-1.png)<!-- -->

```r
        labs(x = "Interval",
             y = "Average Steps",
             title = "Average daily activity pattern")
```

```
## $x
## [1] "Interval"
## 
## $y
## [1] "Average Steps"
## 
## $title
## [1] "Average daily activity pattern"
## 
## attr(,"class")
## [1] "labels"
```
