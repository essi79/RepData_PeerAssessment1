activityData<-read.csv("activity.csv",stringsAsFactors = FALSE)

activityData$date<-as.Date(activityData$date)

stepsPerDay<-with(activityData, tapply(steps, date, sum))

hist(stepsPerDay)

mean(stepsPerDay,na.rm = TRUE)

median(stepsPerDay,na.rm = TRUE)

averageStepsPerInterval<-with(activityData[!is.na(activityData$steps),], tapply(steps, interval, mean))
names(averageStepsPerInterval)<-activityData$interval[1:288]
plot(averageStepsPerInterval,type="l")
averageStepsPerInterval[which.max(averageStepsPerInterval)]

sum(is.na(activityData$steps))
#replace missing value with the mean of the interval
reppedAverageStepsPerInterval<-rep(averageStepsPerInterval,nrow(activityData)/length(averageStepsPerInterval))
NoNAActivityData<-activityData
NoNAActivityData$steps[is.na(activityData$steps)]<-reppedAverageStepsPerInterval[is.na(activityData$steps)]
NoNAstepsPerDay<-with(NoNAActivityData, tapply(steps, date, sum))
hist(NoNAstepsPerDay)

mean(NoNAstepsPerDay,na.rm = TRUE)

median(NoNAstepsPerDay,na.rm = TRUE)

library("dplyr")
dayVector<-c("Weekday","Weekday","Weekday","Weekday","Weekday","Weekend","Weekend")
names(dayVector)<-c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")

NoNAActivityData <- mutate(NoNAActivityData, dayType=as.factor(dayVector[weekdays(date)]))

weekdayAverageStepsPerInterval<-with(NoNAActivityData[NoNAActivityData$dayType=="Weekday",], tapply(steps, interval, mean))
weekendAverageStepsPerInterval<-with(NoNAActivityData[NoNAActivityData$dayType=="Weekend",], tapply(steps, interval, mean))
par(mfrow=c(2,1),mar=c(2,3,1,1))
plot(weekdayAverageStepsPerInterval,type="l",main="Weekday Average steps per interval")
plot(weekdayAverageStepsPerInterval,type="l",main="Weekend Average steps per interval")

