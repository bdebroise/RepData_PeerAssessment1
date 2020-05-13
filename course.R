
unzip("activity.zip")

library(readr)
activity <- read_csv("activity.csv")

str(activity)


mean(is.na(activity$steps))


summary(activity)
