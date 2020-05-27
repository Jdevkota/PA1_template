##Peer-graded Assignment: Course Project 1##

rm(list=ls())

activity = read.csv("C:/Users/JDevkota/OneDrive/Desktop/activity.csv")
head(activity)
##What is mean total number of steps taken per day?##

##1##
totalsteps=sum(activity$steps, na.rm = TRUE)
totalsteps

##2##
totalseday=aggregate(steps~date, data=activity, FUN=sum, na.rm=TRUE)
hist(totalseday$steps)

##3##
totalsedmean=mean(totalseday$steps)
totalsedmedian=median(totalseday$steps)


##What is the average daily activity pattern?##

##1##
fivemavg=aggregate(steps~interval, data=activity, FUN=mean, na.rm=TRUE)
fivemavg
plot(x = fivemavg$interval, y = fivemavg$steps, type = "l",col="red")

##2##
max_pos=which(fivemavg$mean==max(fivemavg$mean))

max_interval=fivemavg[max_pos, 1]

rm(max_pos, fivemavg)

##Imputing missing values##

##1##
missingVals=sum(is.na(activity$steps))
missingVals

##2##
rm(missingVals)
napos=which(is.na(activity$steps))
meanvec=rep(mean(activity$steps, na.rm=TRUE), times=length(napos))

##3##
activity[napos, "steps"]=meanvec
rm(meanvec, napos)
head(activity)

##4a##
totalsted=aggregate(steps~date, data=activity, FUN=sum, na.rm=TRUE)
hist(totalsted$steps)
##4b##
totalstedmean=mean(totalsted$steps)
totalstedmean

totalstemedian=median(totalsted$steps)
totalstemedian

##Are there differences in activity patterns between weekdays and weekends?##

##1##
week= wday(activity$date)
weday=week
for (i in 1:17568) # loop to find the na
{
  if(week[i] == 1)
    weday[i] ='weekend'
  if(week[i] == 2)
    weday[i] ='weekday'
  if(week[i] == 3)
    weday[i] = 'weekday'
  if(week[i] == 4)
    weday[i] ='weekday'
  if(week[i] == 5)
    weday[i] ='weekday'
  if(week[i] == 6)
    weday[i] = 'weekday'
  if(week[i] == 7)
    weday[i] = 'weekend'
}
activity$weekday=weday
weday

##2##

weekday=grep("weekday",activity$weekday)
weekday_frame=activity[weekday,]
weekend_frame=activity[-weekday,]

fiveminavg_weekday=aggregate(steps~interval, data=weekday_frame, FUN=mean, na.rm=TRUE)
plot(x = fiveminavg_weekday$interval, y = fiveminavg_weekday$steps, type = "l",col="green",main="Weekdays")

fiveminavg_weekend=aggregate(steps~interval, data=weekend_frame, FUN=mean, na.rm=TRUE)
plot(x = fiveminavg_weekend$interval, y = fiveminavg_weekend$steps, type = "l",col="red",main="Weekend") 


















