library(ggplot2)
library(readr)
activity <- read_csv("~/activity.csv")

#aggregate by day
bydate <- aggregate(activity["steps"], by=activity["date"], sum)

#Histogram of number of steps by day
hist(bydate$steps, 
     main="Histogram for Number of Steps", 
     xlab="# of Steps",
     ylab="# of Days",
     col="light blue",
     las=1, 
     breaks=5)

#Calculate Mean (Removed NA Values)
mean(bydate$steps, na.rm = TRUE)

#Calculate Median (Removed NA Values)
median(bydate$steps, na.rm = TRUE)

#Interval Aggregate
byinterval <- aggregate(activity["steps"], by=activity["interval"], mean, na.rm = TRUE)

#Plot Time Series
ggplot(byinterval, aes(interval, steps)) + geom_line() +
  xlab("Interval") + ylab("Steps")

#determine interval with max steps
maxsteps <- byinterval[which.max(byinterval$steps),]
maxsteps$interval

#Total NAs
sum(is.na(activity$steps))

#Create new actitity table replacing NA with average by interval
activitynew <- activity

activitynew$steps <- with(activitynew, ave(steps, interval,
                          FUN = function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))))
activitynew

#aggregate new by day
bydatenew <- aggregate(activitynew["steps"], by=activitynew["date"], sum)

#Histogram of number of steps by day
hist(bydatenew$steps, 
     main="Histogram for Number of Steps", 
     xlab="# of Steps",
     ylab="# of Days",
     col="light blue",
     las=1, 
     breaks=5)

#Calculate Mean (Removed NA Values)
mean(bydatenew$steps, na.rm = TRUE)

#Calculate Median (Removed NA Values)
median(bydatenew$steps, na.rm = TRUE)

#Interval Aggregate
byintervalnew <- aggregate(activitynew["steps"], by=activitynew["interval"], mean, na.rm = TRUE)

#Plot Time Series
ggplot(byintervalnew, aes(interval, steps)) + geom_line() +
  xlab("Interval") + ylab("Steps")

#determine interval with max steps
maxstepsnew <- byintervalnew[which.max(byintervalnew$steps),]
maxstepsnew$interval

#Classify Weekend
activity$day <- weekdays(activity$date)
activity$weekend <- revalue(activity$day, 
                            c("Monday" = "Weekday", 
                              "Tuesday" = "Weekday",
                              "Wednesday" = "Weekday",
                              "Thursday" = "Weekday", 
                              "Friday" = "Weekday", 
                              "Saturday" = "Weekend", 
                              "Sunday" = "Weekend"))

#Aggregate by day and By weekend
bydayinterval <- aggregate(steps ~ weekend+interval, data = activity, mean, na.rm = TRUE)

#Plot Facet Weekend Plot
p <- ggplot(bydayinterval, aes(interval, steps)) + geom_line() +
  xlab("Interval") + ylab("Steps")
p + facet_grid(.~weekend)
