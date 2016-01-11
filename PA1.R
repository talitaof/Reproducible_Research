#Reproducible Research: Peer Assessment 1

install.packages(c("ggplot2", "scales", "Hmisc","qplot"))

library(ggplot2)
library(scales)
library(Hmisc)

##Loading and preprocessing the data

if(!file.exists('activity.csv')){
    unzip('activity.zip')
}

activity <- read.csv("activity.csv")
View(activity)

##Process/transform the data (if necessary) into a format suitable for your analysis


##What is mean total number of steps taken per day?
###For this part of the assignment, you can ignore the missing values in the dataset.

###Calculate the total number of steps taken per day

totalNumberStepsByDay <- tapply(activity$steps, activity$date, sum, na.rm=TRUE,simplify = TRUE)

###If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

hist(totalNumberStepsByDay, xlab = "Number of steps", main = "Histogram of the total number of steps taken each day")

###Calculate and report the mean and median of the total number of steps taken per day

mean(totalNumberStepsByDay)
#### [1] 9354.23
median(totalNumberStepsByDay)
#### [1] 10395

##What is the average daily activity pattern?

###Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

activity$interval <- as.factor(activity$interval)
averageNumberSteps <- tapply(activity$steps, activity$interval, sum, na.rm = TRUE,simplify = TRUE)/length(levels(activity$date))

plot(x = levels(activity$interval), y = averageNumberSteps, type = "l", xlab = "Time",ylab = "Number of steps", main = "Average number of steps taken in 5-minute interval across all days")

###Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

names(which.max(averageNumberSteps))
####[1] "835"

##Imputing missing values

###Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
###Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

numberMissingValues <- sum(!complete.cases(activity$steps))
str(numberMissingValues)
####int 2304

###Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
###Create a new dataset that is equal to the original dataset but with the missing data filled in.

originalData <- activity
for (i in 1:length(activity$steps)) {
    if (is.na(activity$steps[i])) {
        for (j in 1:length(averageNumberSteps)) {
            if (as.character(activity$interval[i]) == names(averageNumberSteps[j])) 
                activity$steps[i] = averageNumberSteps[j]
        }
    }
}
cleanData <- activity

###Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

newTotalNumberStepsByDay <- tapply(cleanData$steps, cleanData$date, sum, na.rm=TRUE,simplify = TRUE)
hist(newTotalNumberStepsByDay, xlab = "Number of steps", main = "Histogram of the total number of steps taken each day")

mean(newTotalNumberStepsByDay )
####[1] 10581.01
median(newTotalNumberStepsByDay )
####[1] 10395

####The mean and median total number of steps taken per day are 1.0581 and 1.0395, the difference of mean and median value are -1226.7842 and 0. The shape of two histograms change, especially for the number of steps below 10000. The mean value increase significantly after imputing missing data and the total daily number of steps increase a little.

##Are there differences in activity patterns between weekdays and weekends?

###For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

###Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

Sys.setlocale("LC_TIME","English")
weekday <- weekdays(as.Date(cleanData$date, "%Y-%m-%d"))
for (i in 1:length(weekday))
    if ((weekday[i] == "Saturday") || (weekday[i] == "Sunday"))
        weekday[i] = "weekend"

for (i in 1:length(weekday))
	if (weekday[i] != "weekend")
		weekday[i] = "weekday"

cleanData$weekday <- as.factor(weekday)


###Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

cleanData2 <- split(cleanData, cleanData$weekday)
weekday <- cleanData2$weekday
weekend <- cleanData2$weekend
stepAverageWeekday <- tapply(weekday$steps, weekday$interval, sum, simplify = TRUE)/(length(weekday$weekday)/288)
stepAverageWeekend <- tapply(weekend$steps, weekend$interval, sum, simplify = TRUE)/(length(weekend$weekday)/288)
output <- data.frame(steps = c(stepAverageWeekday, stepAverageWeekend), 
    interval = c(levels(activity$interval), levels(activity$interval)), weekday = as.factor(c(rep("weekday", 
        length(stepAverageWeekday)), rep("weekend", length(stepAverageWeekend)))))
library(lattice)
xyplot(steps ~ interval | weekday, data = output, layout = c(1, 2), ylab = "Number of steps", 
    main = "Average number of steps for all weekday days or weekend days")

