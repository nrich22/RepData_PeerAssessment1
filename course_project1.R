# Nicholas Ricciarelli
# Reproducible Research
# Course Project 1

library(ggplot2)
library(dplyr)
library(lattice)

# read data
data <- read.csv("activity.csv")

# What is mean total number of steps taken per day?
totalSteps <- data %>% group_by(date) %>% summarise(dailySteps = sum(steps, na.rm = TRUE))
png("plot1.png", width=480, height=480, units="px", bg="white")
ggplot(totalSteps, aes(dailySteps)) + geom_histogram(binwidth = 2000) +
  xlab("Total number of steps taken each day") + 
  ylab("Frequency")

stepsMean <- mean(totalSteps$dailySteps, na.rm=TRUE)
stepsMedian <- median(totalSteps$dailySteps, na.rm=TRUE)

message(stepsMean)
message(stepsMedian)

# What is the average daily activity pattern?
intervalSteps <- data %>% 
  group_by(interval) %>%
  summarise(steps = mean(steps, na.rm =TRUE))
png("plot2.png", width=480, height=480, units="px", bg="white")
ggplot(data=intervalSteps, aes(x=interval, y=steps)) +
    geom_line() +
    xlab("5-minute intervals") +
    ylab("Average number of steps taken")

# Imputing missing values
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
message(meanDifference)

# Are there differences in activity patterns between weekdays and weekends?
weekendDays <- c("Saturday", "Sunday")
imputedData$dow = as.factor(ifelse(is.element(weekdays(as.Date(imputedData$date)), weekendDays), "Weekend", "Weekday"))
imputerSteps <- aggregate(steps ~ interval + dow, imputedData, inmputedMean)

png("plot4.png", width=480, height=480, units="px", bg="white")
xyplot(imputedData$steps ~ imputedData$interval|imputedData$dow, main="Average Steps per Day by Interval", xlab="Interval", ylab="Steps", layout=c(1,2), type="l")
dev.off()

