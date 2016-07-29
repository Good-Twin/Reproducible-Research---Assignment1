library(dplyr)
setwd("~/Weiterbildung/Data Science Specialisation/05_Reproducible Research/Assignment 1")


act <- read.csv("activity.csv", sep=",", header=TRUE)

act_sum <- summarise(group_by(act, date), steps_per_day= sum(steps))

#Histogram
barplot(act_sum$steps_per_day, names.arg=act_sum$date, las=2, cex.names=0.8)

#mean and median
act_sum <- mutate(group_by(act_sum, date)
                  , step_mean = mean(steps_per_day,na.rm=TRUE)
                  , step_median = mean(steps_per_day,na.rm=TRUE))


#What is the average daily activity pattern?
act_interval <- summarise(group_by(act, interval), steps_per_day= sum(steps, na.rm=TRUE))
plot(act_interval$interval, act_interval$steps_per_day, type="l" )

(filter(act_interval, steps_per_day == max(act_interval$steps_per_day)) )

#Imputing missing values

sum(!complete.cases(act))


act_means <-rename(summarise(group_by(act, interval), steps_median= median(steps, na.rm=TRUE)),key=interval) 
act_cleaned <- cbind(act,act_means, by.x=act$interval, by.y=act_means$key)
act_cleaned <- group_by(act_cleaned, interval)
act_cleaned <- mutate(act_cleaned, steps = ifelse(is.na(steps),steps_median, steps))

#Histogram
act_sum_cleaned <- summarise(group_by(act_cleaned, date), steps_per_day= sum(steps))
barplot(act_sum_cleaned$steps_per_day, names.arg=act_sum$date, las=2, cex.names=0.8)

#Get weekday from index of the day
weekdays <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

act_cleaned$day <- weekdays[as.POSIXlt(act_cleaned$date)$wday + 1]
act_cleaned <- mutate(act_cleaned, day_type = ifelse(day=='Saturday' | day=='Sunday' ,'weekend', 'weekday'))

act_sum_cleaned <- summarise(group_by(act_cleaned, interval, day_type), steps_per_day= sum(steps))

xyplot(steps_per_day ~ interval | day_type, 
        act_sum_cleaned,
        layout=c(1,2),
        type ='l',
        ylab="Number of steps",
        xlab="Interval")




