# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
library(plyr)
options(scipen=999)
zipPath <- "activity.zip";
filename <- "activity.csv";
csvfile <- unz (zipPath, filename);
raw_data <- read.csv(csvfile)
clean_data <- raw_data[!is.na(raw_data$steps), ]
clean_data <- clean_data[clean_data$steps != 0, c('date', 'steps')]

# group data by date and sum the steps
aggregate_data <- aggregate(x=clean_data$steps, by=list(clean_data$date), FUN = sum)
colnames(aggregate_data) <- c('date', 'total_steps')

# group by number of steps and count number of occurrences
aggregate_steps <- count(aggregate_data, "total_steps")
colnames(aggregate_steps) <- c('steps', 'frequency')
aggregate_data <- aggregate_data[order(aggregate_data$total_steps), ]

mean_steps = mean(aggregate_data$total_steps)
median_steps = median(aggregate_data$total_steps)
```

## What is mean total number of steps taken per day?


```r
library(ggplot2)
p1 <- ggplot(aggregate_data, aes(x=aggregate_data$total_steps))
p1 <- p1 + geom_histogram(fill="red", color="black", binwidth=1000) + ggtitle("Frequency of total steps taken per day") + xlab('Total number of steps per day') + ylab('Frequency')
print (p1)
```

![plot of chunk total_steps](figure/total_steps.png) 

1. The mean total number of steps taken per day is 10766.1887.
2. The median total number of steps taken per day is 10765.

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
