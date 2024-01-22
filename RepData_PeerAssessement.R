#Code for reading in the dataset and/or processing the data
file<-"C:\\Users\\dell\\Downloads\\repdata_data_activity.zip"
unzip(file)
activity<-read.csv("activity.csv")
#Histogram of the total number of steps taken each day
activity_NoNas<-na.omit(activity)
total_nb_steps<-activity_NoNas %>% select(steps,date) %>% group_by(date)%>% mutate(date=factor(date))%>% summarize(total_steps=sum(steps))
hist(total_nb_steps$total_steps,xlab = "Number of steps",col = "green")
#Mean and median number of steps taken each day
mean(total_nb_steps$total_steps)
median(total_nb_steps$total_steps)
#Time series plot of the average number of steps taken
average_daily_activity<-activity_NoNas %>%select(interval,steps) %>%group_by(interval)%>% mutate(interval=factor(interval)) %>% summarize(average_nb_steps=mean(steps))
plot(x= average_daily_activity$interval,y=average_daily_activity$average_nb_steps,type="l",ylab="average nb of steps",xlab="5 min interval")
#The 5-minute interval that, on average, contains the maximum number of steps
which.max(average_daily_activity$average_nb_steps)
average_daily_activity$interval[104]
#Calculate number of missing values
summary(activity)
sum(is.na(activity))
#Code to describe and show a strategy for imputing missing data
activityImputed <- activity
meanByInterval <- aggregate(steps ~ interval, activity, mean)
#Create a new dataset that is equal to the original dataset but with the missing data filled in.
activityImputed <- merge(x = activity, y = meanByInterval, by = "interval", all.x = TRUE)
activityImputed <- data.frame(
  ifelse(is.na(activityImputed$steps.x), activityImputed$steps.y, activityImputed$steps.x),
  activityImputed$date,
  activityImputed$interval
)
colnames(activityImputed) <- c("steps", "date", "interval")
#Histogram of the total number of steps taken each day after missing values are imputed
stepsByIntervalImputed <- aggregate(steps ~ date, activityImputed, sum)
hist(stepsByIntervalImputed$steps, main = paste("Total number of steps taken each day"), col="blue", xlab="Number of Steps")
#Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
activityImputed$noday <- as.POSIXlt(activityImputed$date)$wday
activityImputed$daytype <- ifelse(activityImputed$noday %in% c(0,6), 'weekend', 'weekday')
stepsByIntervalDaytype <- aggregate(steps ~ interval+daytype,activityImputed, mean)
ggplot(data=stepsByIntervalDaytype, aes(x=interval, y=steps , group=daytype, colour=daytype   )
) +
  geom_line() +
  ylab(expression('Average step')) +
  ggtitle('Time series plot of the average number of steps taken')  +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
  )

