install.packages("timeDate")
library(timeDate)
library(ggplot2)

## ----leer data de archivo local en mi folder------------------------------------------------------------
fileurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file( fileurl, "activity.zip")  
unzip(zipfile="activity.zip")

activity <- read.csv("activity.csv")


## ----numero de pasos medio tomado por día---------------------------------------------------
#cuento total de steps mediante sum y los calculo por date, remuevo NA
steps.date <- aggregate(steps ~ date, data = activity, FUN = sum)
barplot(steps.date$steps, names.arg = steps.date$date, xlab = "date", ylab = "total steps",main = "Number of steps by date.")

mean(steps.date$steps, na.rm=TRUE)
median(steps.date$steps, na.rm=TRUE)


## ------------------------------------------------------------------------
steps.interval <- aggregate(steps ~ interval, data = activity, FUN = mean)
plot(steps.interval, type = "l")


## ------------------------------------------------------------------------
steps.interval$interval[which.max(steps.interval$steps)]


## ----how_many_missing----------------------------------------------------
sum(is.na(activity))

## ------------------------------------------------------------------------


activity.filled <- merge(activity, steps.interval, by = "interval", suffixes = c("", ".y"))
nas <- is.na(activity.filled$steps)
activity.filled$steps[nas] <- activity.filled$steps.y[nas]
activity.filled <- activity.filled[, c(1:3)]

sum(is.na(activity.filled))

## ------------------------------------------------------------------------
steps.date.filled <- aggregate(steps ~ date, data = activity.filled, FUN = sum)
barplot(steps.date.filled$steps, names.arg = steps.date.filled$date, xlab = "date", ylab = "total steps",main = "Number of steps by date.")
mean(steps.date.filled$steps, na.rm=TRUE)
median(steps.date.filled$steps, na.rm=TRUE)



weeked <- ifelse(isWeekend((activity.filled$date)), "weekend", "weekday")
final <- data.frame(activity.filled,weeked)
average_steps <- aggregate(steps ~ weeked + interval, data = final,mean)

qplot(interval, steps, data = average_steps, facets = weeked~., geom = "line")
qplot(interval, steps, data = average_steps, color = weeked, geom = "line")
