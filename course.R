
unzip("activity.zip")

library(readr)
activity <- read_csv("activity.csv")

str(activity)
summary(activity)

mean(is.na(activity$steps))


summary(activity)


act0 <- with(activity,tapply(steps,date,mean,na.rm=T))
act0
summary(act0)
class(act0)


df0 <- data.frame(date=names(act0),mean=act0)
hist(df0$mean)
barplot(df0$mean)
plot(df0)




agg = aggregate(activity,
                by = list(activity$date),
                FUN = mean,
                na.rm=TRUE)
agg
class(agg)
barplot(agg$steps)







# deuxieme question
agg = aggregate(activity,
                by = list(activity$interval),
                FUN = mean,
                na.rm=TRUE)

summary(agg)
agg


max <- subset(agg,agg$steps==206.170)


plot(x=agg$interval,y=agg$steps,type="l",xlab = "Intervals",ylab="Steps mean")

points(835,206.170,pch=20,col="red")



# troisieme





