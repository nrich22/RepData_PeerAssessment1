---
title: "Reproducible Research: Peer Assessment 1"
author: "Nicholas Ricciarelli"
date: "August 12, 2022"
output: 
  html_document:
    number_sections: true
    keep_md: true
---


## Loading and preprocessing the data

```r
library(ggplot2)
library(dplyr)
library(lattice)

data <- read.csv("activity.csv")
```


## What is mean total number of steps taken per day?

```r
totalSteps <- data %>% group_by(date) %>% summarise(dailySteps = sum(steps, na.rm = TRUE))
png("plot1.png", width=480, height=480, units="px", bg="white")
ggplot(totalSteps, aes(dailySteps)) + geom_histogram(binwidth = 2000) +
  xlab("Total number of steps taken each day") + 
  ylab("Frequency")

stepsMean <- mean(totalSteps$dailySteps, na.rm=TRUE)
stepsMedian <- median(totalSteps$dailySteps, na.rm=TRUE)

message(stepsMean)
```

```
## 9354.22950819672
```

```r
message(stepsMedian)
```

```
## 10395
```


## What is the average daily activity pattern?

```r
intervalSteps <- data %>% 
  group_by(interval) %>%
  summarise(steps = mean(steps, na.rm =TRUE))
png("plot2.png", width=480, height=480, units="px", bg="white")
ggplot(data=intervalSteps, aes(x=interval, y=steps)) +
    geom_line() +
    xlab("5-minute intervals") +
    ylab("Average number of steps taken")
message(835)
```

```
## 835
```


## Imputing missing values

```r
missingData <- !complete.cases(data)
imputedData <- data %>%
  mutate(
    steps = case_when(
      is.na(steps) ~ intervalSteps$steps[match(data$interval, intervalSteps$interval)],      
      TRUE ~ as.numeric(steps)
    ))
imputedTotalSteps <- imputedData %>% group_by(date) %>% summarise(dailySteps = sum(steps))
png("plot3.png", width=480, height=480, units="px", bg="white")
ggplot(imputedTotalSteps, aes(dailySteps)) + 
  geom_histogram(binwidth = 2000) + 
  xlab("Total number of steps taken each day") + 
  ylab("Frequency")

inmputedMean <- mean(imputedTotalSteps$dailySteps, na.rm=TRUE)
imputedMedian <- median(imputedTotalSteps$dailySteps, na.rm=TRUE)

meanDifference <- inmputedMean - stepsMean 
medianDifference <- imputedMedian - stepsMedian

message(meanDifference)
```

```
## 1411.95917104856
```

```r
message(meanDifference)
```

```
## 1411.95917104856
```


## Are there differences in activity patterns between weekdays and weekends?

```r
weekendDays <- c("Saturday", "Sunday")
imputedData$dow = as.factor(ifelse(is.element(weekdays(as.Date(imputedData$date)), weekendDays), "Weekend", "Weekday"))
imputerSteps <- aggregate(steps ~ interval + dow, imputedData, inmputedMean)
```

```
## Error in get(as.character(FUN), mode = "function", envir = envir): object 'FUN' of mode 'function' was not found
```

```r
png("plot4.png", width=480, height=480, units="px", bg="white")
xyplot(imputedData$steps ~ imputedData$interval|imputedData$dow, main="Average Steps per Day by Interval", xlab="Interval", ylab="Steps", layout=c(1,2), type="l")
```
