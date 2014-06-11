library(ggplot2)

zipPath <- "activity.zip";
filename = "activity.csv";
csvfile = unz (zipPath, filename);
raw_data = read.csv(csvfile)
clean_data = raw_data[!is.na(raw_data$steps), ]
non_zero_data = clean_data[clean_data$steps != 0, c('date', 'steps')]

x <- clean_data[clean_data$date=='2012-10-03',]
print(x)