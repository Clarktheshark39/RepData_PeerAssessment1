## Coursera, Reproducible Research, Week 2, Project:
## Old school download to computer first, then read into program.
# on PC
setwd("C:/Users/cporter01/Downloads") 
# on Mac
setwd("~/Downloads")
activity <- read.csv("activity.csv")

## Attempt to download data directly from the link/website...does not work currently.
temp <- tempfile()
URL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(URL, temp)
activity <- read.table(unz(temp, "repdata%2Fdata%2Factivity.zip"))
unlink(temp)

## Assignment:
## 1. Code for reading in the dataset and/or processing the data
## 2. Histogram of the total number of steps taken each day
## 3. Mean and median number of steps taken each day
## 4. Time series plot of the average number of steps taken
## 5. The 5-minute interval that, on average, contains the maximum number of steps
## 6. Code to describe and show a strategy for imputing missing data
## 7. Histogram of the total number of steps taken each day after missing values are imputed
## 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
## 9. All of the R code needed to reproduce the results (numbers, plots, etc.) in the report

## Process the data to prepare it for analysis. 
library(dplyr)
library(ggplot2)

## The number of steps taken per day (steps per day or "spd")
spd <- summarize(group_by(activity, date), steps = sum(steps, na.rm = T))
## The number of steps per day is logged in the 'steps' column of spd. There are many missing and 0 values in the data.

## Make a histogram of the steps per day. 
hist(spd$steps, breaks = 15, xlab = "Steps per Day", main = "Steps Per Day", col = "dodgerblue")

## Calculate the mean and median of the total number of steps per day.
summary(spd$steps)
## Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0    6778   10400    9354   12810   21190 

## A time series plot for the average number of steps taken at each 5 minute interval across all days. 
avgsteps <- summarize(group_by(activity, interval), meansteps = mean(steps, na.rm = T))

ggplot(data = avgsteps, aes(interval, meansteps)) +
  geom_line(lwd = 1, color = "dodgerblue") +
  xlab("Interval") + 
  ylab("Average Number of Steps") +
  ggtitle("Average Number of Steps Across All Days") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

## Which interval (on average) contains the highest number of steps per day? 
avgsteps[which(avgsteps$meansteps == max(avgsteps$meansteps)), ]
##   interval    meansteps
##     835       206.1698

## Report the total number of NAs in the dataset. 
## In the steps column:
sum(is.na(activity$steps))
## In the interval column:
sum(is.na(activity$interval))
## In the date column:
sum(is.na(activity$date))

## Replace the NA values in the data. I will replace each NA in the steps column with the average
## number of steps across all dates for the given interval. activityrm is the data without NAs.
activityrm <- activity
for (i in 1:nrow(activity)){
  if (is.na(activity$steps[i])){
    activityrm$steps[i] <- avgsteps[which(avgsteps$interval == activity$interval[i]), 2]
  }
}
## Plot a histogram off the data without NAs. This shows the new Steps per day.
activityrm$steps <- as.numeric(activityrm$steps)
spdrm <- summarize(group_by(activityrm, date), steps = sum(steps))
hist(spdrm$steps, breaks = 15, xlab = "Steps per Day", main = "Steps Per Day", col = "dodgerblue")
summary(spdrm$steps)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 41    9819   10770   10770   12810   21190 

## Create a new factor variable classifying each day as either a weekday or weekend. 
library(lubridate)
activityrm$day <- wday(activityrm$date)
activityrm$wkdy <- ifelse(activityrm$day == 7 | activityrm$day == 1, "Weekend", "Weekday")
activityrm$wkdy <- factor(activityrm$wkdy)

## Plot in two panels the average number of steps taken by interval. One panel should be weekdays and the other weekend days. 
actrmavg <- aggregate(steps ~ interval + wkdy, data=activityrm, mean)
ggplot(actrmavg, aes(interval, steps)) + 
  geom_line() + 
  facet_grid(wkdy ~ .) +
  xlab("5-minute Interval") + 
  ylab("Average Number of Steps")



