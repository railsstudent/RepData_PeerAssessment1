library(ggplot2)
library(plyr)

zipPath <- "activity.zip";
filename <- "activity.csv";
csvfile <- unz (zipPath, filename);
raw_data <- read.csv(csvfile)
clean_data <- raw_data[!is.na(raw_data$steps), ]

# group data by date and sum the steps
aggregate_data <- aggregate(x=clean_data$steps, by=list(clean_data$date), FUN = sum)
colnames(aggregate_data) <- c('date', 'total_steps')
aggregate_data <- aggregate_data[order(aggregate_data$total_steps), ]

mean_steps = mean(aggregate_data$total_steps)
median_steps = median(aggregate_data$total_steps)

# aggregate steps by intervals
aggregate_interval <- aggregate(x=clean_data$steps, by=list(clean_data$interval), FUN = sum)
colnames(aggregate_interval) <- c('interval', 'total_steps')

aggregate_num_date <- count(clean_data, "interval")
colnames(aggregate_num_date) <- c("interval","num_dates")

aggregate_interval <- cbind(aggregate_interval, aggregate_num_date$num_dates)
colnames(aggregate_interval) <- c("interval","total_steps", "num_dates")
aggregate_interval$avg_steps <- aggregate_interval$total_steps / aggregate_interval$num_dates

max_interval <- aggregate_interval[aggregate_interval$avg_steps == max(aggregate_interval$avg_steps), c('interval')]

num_missing_data_row <- nrow(raw_data[is.na(raw_data$steps), ])

new_ds <- raw_data
a1 <- new_ds[is.na(new_ds$steps), ]
a2 <- new_ds[!is.na(new_ds$steps), ]
mat <- as.matrix(a1)

for (i in 1:nrow(mat)) {
 x <- aggregate_interval[aggregate_interval$interval == as.integer(mat[i, "interval"]), "avg_steps" ]
 mat[i, "steps"] <- x
}

#a3 <- as.data.frame(mat)
#new_ds <- rbind(a2,a3)
new_ds <- rbind(a2,as.data.frame(mat))
new_ds$interval <- as.integer(new_ds$interval)
new_ds <- new_ds[order(new_ds$date, as.integer(new_ds$interval)),]
new_ds$steps <- as.numeric(new_ds$steps)
new_ds$wday <- as.POSIXlt(new_ds$date)$wday
new_ds$factor <- ifelse(new_ds$wday %in% c(1,2,3,4,5), 'weekday', 'weekend')

agg_data_input <- aggregate(x=new_ds$steps, by=list(new_ds$date), FUN = sum)
colnames(agg_data_input) <- c('date', 'total_steps')

mean_steps_input = mean(agg_data_input$total_steps)
median_steps_input = median(agg_data_input$total_steps)

# sum steps by weekday/weekend and interval
agg_data_wday <- aggregate(x=new_ds$steps, by=list(new_ds$factor, new_ds$interval), 
                           FUN = sum)
colnames(agg_data_wday) <- c('wday', 'interval', 'total_steps')
agg_data_wday <- agg_data_wday[order(agg_data_wday$wday,  agg_data_wday$interval), ]
colnames(agg_data_wday)[3] <- 'sss'

