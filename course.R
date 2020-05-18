act1$day  
unzip("activity.zip")

library(readr)
activity <- read_csv("activity.csv")

str(activity)
summary(activity)

head(activity)

mean(is.na(activity$steps))


activity <- read.csv("activity.csv")
head(activity)



library(dplyr)
act0 <- activity %>% group_by(date) %>% summarise(total.steps = sum(steps,na.rm=TRUE))
mean(act0$total.steps,na.rm=TRUE)
median(act0$total.steps,na.rm=TRUE)


barplot(act0$total.steps)





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



act1 <- activity %>% group_by(interval) %>% summarise(mean.steps = mean(steps,na.rm=TRUE))
plot(x=act1$interval,y=act1$mean.steps,type="l",xlab = "Intervals",ylab="Steps mean")


max(act1$mean.steps)


max <- subset(agg,agg$steps==206.170)


plot(x=agg$interval,y=agg$steps,type="l",xlab = "Intervals",ylab="Steps mean")



points(835,206.170,pch=20,col="red")



# troisieme
# https://stackoverflow.com/questions/50919161/how-to-simply-count-number-of-rows-with-nas-r/50919195

head(rowSums(is.na(activity)))
rowSums(is.na(activity))
        

head(colSums(is.na(activity)))

sum(is.na(activity))

x0 <- is.na(activity$date)
x0

#�act0 <- apply(activity,activity$steps=="NAN",activity$steps<-agg[agg$interval==] )


act0 <- activity

for (i in 1:17568) {
    if (is.na(act0[i,1])){
        agg0<- subset(agg,agg$interval == act0[i,3])
       act0[i,1] <- agg0$steps
    }
}



act2 <- activity

for (i in 1:17568) {
    if (is.na(act2[i,1])){
        val<- subset(act1,act1$interval == act2[i,3])
        act2[i,1] <- val$mean.steps
    }
}


act3 <- act2 %>% group_by(date) %>% summarise(total.steps = sum(steps,na.rm=TRUE))


mean(act3$total.steps,na.rm=TRUE)

median(act3$total.steps,na.rm=TRUE)

barplot(act3$total.steps)





act1 <- subset(act0,act0$interval==0)


# question 4


act1 <- activity




library(lubridate)



library(dplyr)

act3 <- tbl_df(activity)

act4 <- act3 %>% group_by(interval) %>% summarise(mean.steps = mean(steps,na.rm=TRUE))


library(lubridate)

act2$day <- wday(act2$date)

act2$w.days <- ifelse(act2$day %in% 2:6,"weekday","weekend")
act2$w.days <- as.factor(act2$w.days)


act4 <- act2 %>% group_by(w.days,interval) %>% summarise(mean.steps = mean(steps,na.rm=TRUE))


library(lattice)
with(act4, xyplot(act4$mean.steps ~ act4$interval | act4$w.days , type = "l",xlab="Interval",ylab = "Mean steps"))




agg = aggregate(act0,
                by = list(act0$interval,act0$day1),
                FUN = mean,
                na.rm=TRUE)



plot(act0$steps,act0$day1)

library(lattice)
with(agg, xyplot(agg$steps ~ agg$Group.1 | agg$Group.2 , type = "l"))

# plot(x,y,ylim=c(0,10),xlim=c(0,5))type = "l"

# wday("2012-10-01")

#1 ocotbre 2012 un lundi



