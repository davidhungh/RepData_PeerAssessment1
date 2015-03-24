if (!"lubridate" %in% installed.packages()) install.packages("lubridate") 
require(lubridate)

if (!"dplyr" %in% installed.packages()) install.packages("dplyr") 
require(dplyr)

# Loading and preprocessing the data
raw_activity <- read.csv("activity.csv")

# Converte to date format

activity <- raw_activity 

# activity$date <- as.Date(as.character(raw_activity$date), format = "%d/%m/%Y")

activity$date <- ymd(activity$date)

#######################################################
# What is the mean total number of steps taken per day?
#######################################################
# 1- Calculate the total number of steps taken per day
by_date <- group_by(activity, date)

# Total number of steps taken per day
total_steps<- summarize(by_date,sum(steps, na.rm =T))
names(total_steps) <- c("date", "sum_steps")
head(total_steps)

# histogram vs barplot?
# 2- Make a histogram of "the total number of steps taken each day"

# Prepare plot
title <- "Histogram of the total number of steps taken each day"
xLabel <- "Total number of steps taken each day"
par(mfrow = c(1,1))

# Make plot
hist(total_steps$sum_steps,
     main = title,
     xlab = xLabel,
     col = "red")

# 3- Calculate and report the mean and median total number of steps taken per day
mean_steps = summarize(total_steps, mean(sum_steps, na.rm = T))
# or mean(total_steps$sum_steps, na.rm =T)
median_steps = summarize(total_steps, median(sum_steps, na.rm = T))
# or median(total_steps$sum_steps, na.rm = T)

#######################################################
# What is the average daily activity pattern?
#######################################################

# 1- Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and 
# the average number of steps taken, averaged across all days (y-axis)

average_steps<- summarize(by_date,mean(steps, na.rm =T))
names(average_steps) <- c("date", "mean_steps")

# plot(activity$interval, activity$steps, type="l", xlab ="", ylab = "steps")
plot(average_steps$date, average_steps$mean_steps, type="h", xlab ="", 
     ylab ="Averaged steps every 5 minutes")

by_interval <- group_by(activity, interval)
averagedSteps_byInterval <- summarize(by_interval,mean(steps, na.rm=T))
names(averagedSteps_byInterval) <- c("interval", "mean_steps")

plot(averagedSteps_byInterval$interval, averagedSteps_byInterval$mean_steps, type="l", 
     xlab ="Identifier for the 5-minute interval", 
     ylab ="Averaged number of steps taken, averaged across all days")

# 2- Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?

temp <- filter(averagedSteps_byInterval, 
                         mean_steps == max(averagedSteps_byInterval$mean_steps))
theMaxInterval <- temp[1,1]

#######################################################
# Inputing missing value
#######################################################

# 1- Calculate and report the total number of missing values in the dataset 
# (i.e. the total number of rows with NAs)

# list rows of data that have missing values 
rows_of_na <- activity[!complete.cases(activity),]

# The total number of rows with NAs
nof_row <- nrow(rows_of_na)

# 2 -Devise a strategy for filling in all of the missing values in the dataset. 
# Use the mean for that 5-minute interval, averaged over all days

new_activity <- activity

for (i in 1:nof_row) {
  
  # Get the date and the identifier for the row that has the missing value
  na_date = rows_of_na[i,"date"]
  na_identifier = rows_of_na[i,"interval"]
  
  # 
  # Get the averaged value of the 5-minute identifier
  #
  # get the logical vector where there is one True (all others are False)
  a_log = averagedSteps_byInterval$interval == na_identifier 
  a_row = averagedSteps_byInterval[a_log, ] # get the row (1 row)
  a_average = a_row[1,2]
    
  # 3- Create a new dataset that is equal to the original dataset but with the 
  # missing data filled in.
  new_activity$steps[new_activity$date==na_date & new_activity$interval == na_identifier] <- a_average  
}

# 4- Make a histogram of the total number of steps taken each day and Calculate 
# and report the mean and median total number of steps taken per day. Do these 
# values differ from the estimates from the first part of the assignment? 
# What is the impact of imputing missing data on the estimates of the total daily 
# number of steps?

new_by_date <- group_by(new_activity, date)

# Total number of steps taken per day
new_total_steps<- summarize(new_by_date,sum(as.numeric(steps), na.rm =T))
names(new_total_steps) <- c("date", "sum_steps")
head(new_total_steps)

# Prepare plot
new_title <- "New Histogram of the total number of steps taken each day"
xLabel <- "Total number of steps taken each day"
par(mfrow = c(1,1))

# 2- Make a histogram of the total number of steps taken each day
hist(new_total_steps$sum_steps,
     main = new_title,
     xlab = xLabel,
     col = "red")

# 3- Calculate and report the mean and median total number of steps taken per day
new_mean_steps = summarize(new_total_steps, mean(sum_steps, na.rm = T))
new_median_steps = summarize(new_total_steps, median(sum_steps, na.rm = T))

###########################################################################
# Are there differences in activity patterns between weekdays and weekends?
###########################################################################

# 1- Create a new factor variable in the dataset with two levels – “weekday” 
# and “weekend” indicating whether a given date is a weekday or weekend day.
weekdayLevels <-c("weekday", "weekend");

new_activity$day <- wday(new_activity$date, label = T)
new_activity$day <- as.character(new_activity$day)

nof_row = nrow(new_activity)
for (i in 1:nof_row) {
  if ((new_activity[i,"day"] == "Sat") | (new_activity[i,"day"] == "Sun")) 
    new_activity[i,"day"] <- weekdayLevels[2]      
  else 
      new_activity[i,"day"] <- weekdayLevels[1]
}


# 2-Make a panel plot containing a time series plot (i.e. type = "l") of the 
# 5-minute interval (x-axis) and the average number of steps taken, averaged 
# across all weekday days or weekend days (y-axis). 

by_weekdays_interval <- group_by(new_activity,day, interval)
average_steps_weekdays_weekends <- summarize(by_weekdays_interval, mean(as.numeric(steps)))
names(average_steps_weekdays_weekends) <- c("day", "interval", "mean_steps")

if (!"ggplot2" %in% installed.packages()) install.packages("ggplot2") 
require(ggplot2)

p1 <- ggplot(data=avSteps, aes(x=interval, y=mean_steps, color=mean_steps)) + 
  facet_grid(day~.) + 
  geom_line() +   
  ylab("Average number of steps taken per interval") + 
  ggtitle("Activity patterns of weekdays and weekends") +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  theme(axis.title.x = element_text(face="bold", size=15))
  





