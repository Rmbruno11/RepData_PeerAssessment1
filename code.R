# Loading and preprocessing the data
# Show any code that is needed to
# Load the data (i.e. read.csv())
# Process/transform the data (if necessary) into a format suitable for your analysis

if(!file.exists("activity.csv")) unzip("activity.zip")

data <- read.csv("activity.csv", header=TRUE, sep = ",", na.strings = "NA")

# What is mean total number of steps taken per day?
# For this part of the assignment, you can ignore the missing values in the dataset.
# 
# Calculate the total number of steps taken per day

steps_pday <- aggregate(steps ~ date, data, sum)

# Make a histogram of the total number of steps taken each day
library(ggplot2)
ggplot(steps_pday, aes(x=steps)) + geom_histogram(binwidth = 1000, color="black", fill="red") 
 
# Calculate and report the mean and median of the total number of steps taken per day
mean(steps_pday$steps)
median(steps_pday$steps)

# What is the average daily activity pattern?
# 
# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

daily_pattern <- aggregate(steps ~ interval, data, median)
plot(daily_pattern, type="l", col="red")

# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
daily_pattern[daily_pattern$steps==max(daily_pattern$steps),]

 
# Imputing missing values
# 
# Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
# 
# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

table(is.na(data))
 
# Devise a strategy for filling in all of the missing values in the dataset. 
# I take the mean for that 5-minute interval
data_aux <- merge(data, daily_pattern, by="interval", all.x = T)

# Create a new dataset that is equal to the original dataset but with the missing data filled in.
data2 <- data
data2[is.na(data2$steps), "steps"] <- data_aux[is.na(data_aux$steps.x), "steps.y"]

# Make a histogram of the total number of steps taken each day and Calculate and report the mean 
# and median total number of steps taken per day. Do these values differ from the estimates from 
# the first part of the assignment? What is the impact of imputing missing data on the estimates 
# of the total daily number of steps?

steps_pday2 <- aggregate(steps ~ date, data2, sum)

par(mfcol=c(1,2))
hist(steps_pday$steps, col = "red", main = "Histogram with NA", xlab = "Steps")
hist(steps_pday2$steps, col = "blue", main = "Histogram without NA", xlab = "Steps")

mean(steps_pday2$steps)
median(steps_pday2$steps)
mean(steps_pday$steps)
median(steps_pday$steps)

# Are there differences in activity patterns between weekdays and weekends?
# 
# For this part the weekdays() function may be of some help here. Use the dataset with the 
# filled-in missing values for this part.
# 
# Create a new factor variable in the dataset with two levels - "weekday" and "weekend" 
# indicating whether a given date is a weekday or weekend day.
data2$tday <- as.factor(ifelse(as.numeric(format(as.Date(data2[,2]),"%w")) %in% c(0,6), "weekend", "weekday"))

# Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
# and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
# See the README file in the GitHub repository to see an example of what this plot should look like 
# using simulated data.

data_weekends <- aggregate(steps ~ interval, data2[data2$tday=="weekend",], median)
data_weedays <- aggregate(steps ~ interval, data2[data2$tday=="weekday",], median)                        
        
par(mar = rep(2, 4))
par(mfcol=c(2,1))
plot(data_weedays, type="l", col="red", main="Weekdays")
plot(data_weekends, type="l", col="blue", main="Weekends")