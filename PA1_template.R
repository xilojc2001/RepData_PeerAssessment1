
## Unzip de file and extract the content on a CSV file
unzip(zipfile="data/repdata_data_activity.zip")
data <- read.csv("activity.csv")

## Use te ggplot library, for plot the Histogram
library(ggplot2)
dfsteps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
qplot(dfsteps, binwidth=1000, xlab="Number of steps per day", fill = I("blue"))
mean(dfsteps, na.rm=TRUE)
median(dfsteps, na.rm=TRUE)

## Create a time series plot of the 5-minute interval and the average number of 
## steps taken
library(ggplot2)
averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=averages, aes(x=interval, y=steps)) + geom_line() +
       xlab("5-minute interval") + ylab("Average number of steps taken")

## Identify interval with max average number of steps
averages[which.max(averages$steps),]

## Report the total missing values
missing <- is.na(data$steps)
table(missing)

## Replace the missing values with the mean value for the 5-minute interval
filldata <- function(steps, interval) {
        filled <- NA
        if (is.na(steps))
            filled <- (averages[averages$interval==interval, "steps"])
        else
            filled <- c(steps)
        return(filled)
}
filleddata <- data
filleddata$steps <- mapply(filldata, filleddata$steps, filleddata$interval)

## Create a new Histogram with the missing data filled
totalsteps <- tapply(filleddata$steps, filleddata$date, FUN=sum)
qplot(totalsteps, binwidth=1000, xlab="Number of steps per day", fill = I("red"))
mean(totalsteps)
median(totalsteps)

## Identify the kind of weekday for the dates
kindofweekday <- function(date) {
        day <- weekdays(date)
        if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
                return("weekday")
        else if (day %in% c("Saturday", "Sunday"))
                return("weekend")
        else
                stop("invalid date")
}
filleddata$date <- as.Date(filleddata$date)
filleddata$day <- sapply(filleddata$date, FUN=kindofweekday)

## Create a plot to show the comparison between steps on weekdays and weekends
averages <- aggregate(steps ~ interval + day, data=filleddata, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
        xlab("5-minute interval") + ylab("Number of steps")