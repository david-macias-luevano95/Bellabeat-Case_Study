#install.packages('tidyverse')
#install.packages('skimr')
#install.packages('cowplot')
#install.packages("plotly")
library(plotly)
library(tidyverse) #wrangle data
library(dplyr) #clean data
library(lubridate)  #wrangle date attributes
library(skimr) #get summary data
library(ggplot2) #visualize data
library(cowplot) #grid the plot
library(readr) #save csv 
library(plotly) #pie chart
library(tidyr)

getwd()
setwd('C:/Users/davit/Documents/practice_case/practice_case')



#take five principal tables for analisis and combine data in one data frame

activity = read.csv('dailyActivity_merged.csv')
calories = read.csv('dailyCalories_merged.csv')
intensities = read.csv('hourlyIntensities_merged.csv')
sleep = read.csv('sleepDay_merged.csv')
weight = read.csv('weightLogInfo_merged.csv')
hourly_step <- read.csv("hourlySteps_merged.csv")



#cleaning Data for Nan and clean spaces

dim(sleep)
sum(is.na(sleep))
sum(duplicated(sleep))
sleep <- sleep[!duplicated(sleep), ]

n_distinct(sleep$Id)
n_distinct(activity$Id)
n_distinct(calories$Id)


sum(is.na(activity))
sum(is.na(sleep))
sum(is.na(weight))

sum(duplicated(activity))
sum(duplicated(sleep))
sum(duplicated(weight))



######################################################################


#Add a new column for the weekdays
activity <- activity %>% mutate( Weekday = weekdays(as.Date(ActivityDate, "%m/%d/%Y")))

merged1 <- merge(activity,sleep,by = c("Id"), all=TRUE)
merged_data <- merge(merged1, weight, by = c("Id"), all=TRUE)

#Order from Monday to Sunday for plot later
merged_data$Weekday <- factor(merged_data$Weekday, levels= c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

merged_data[order(merged_data$Weekday), ]

#Save CSV for Tableau presentation
write_csv(merged_data, "merged_data.csv")

#Check for NA and duplicates in merged data. 
sum(is.na(merged_data))
sum(duplicated(merged_data))
n_distinct(merged_data$Id)

#count the na data
n_distinct(activity$Id)
n_distinct(sleep$Id)
n_distinct(weight$Id)


#grafico diring the week
  
ggplot(data=merged_data, aes(x=Weekday))+
  geom_bar(fill="steelblue")+
  labs(title="Data Recording During the Week")

ggplot(data=merged_data, aes(x=Weekday, y=TotalSteps, fill=Weekday))+ 
  geom_bar(stat="identity", fill="steelblue")+
  labs(title="More Steps on Saturday", y="Total Steps")

ggplot(data=merged_data, aes(x=Weekday, y=TotalMinutesAsleep, fill=Weekday))+ 
  geom_bar(stat="identity", fill="steelblue")+
  labs(title="More Steps on Saturday", y="TotalMinuteAslpeep")

#suma y promedio de todos los datos del merge data

merged_data %>%
  dplyr::select(Weekday,
                TotalSteps,
                TotalDistance,
                VeryActiveMinutes,
                FairlyActiveMinutes,
                LightlyActiveMinutes,
                SedentaryMinutes,
                Calories,
                TotalMinutesAsleep,
                TotalTimeInBed,
                WeightPounds,
                BMI
  ) %>%
  summary()






#Active users
active_users <- activity %>%
  filter(FairlyActiveMinutes >= 21.4 | VeryActiveMinutes>=10.7) %>% 
  group_by(Id) %>% 
  count(Id) 


#chanege the percentage for grafic
total_minutes <- sum(activity$SedentaryMinutes, activity$VeryActiveMinutes, activity$FairlyActiveMinutes, activity$LightlyActiveMinutes)
sedentary_percentage <- sum(activity$SedentaryMinutes)/total_minutes*100
lightly_percentage <- sum(activity$LightlyActiveMinutes)/total_minutes*100
fairly_percentage <- sum(activity$FairlyActiveMinutes)/total_minutes*100
active_percentage <- sum(activity$VeryActiveMinutes)/total_minutes*100

#Pie charts  clasified level in sedentary percentage fairly 
percentage <- data.frame(
  level=c("Sedentary", "Lightly", "Fairly", "Very Active"),
  minutes=c(sedentary_percentage,lightly_percentage,fairly_percentage,active_percentage)
)


#pie chart grafic 
plot_ly(percentage, labels = ~level, values = ~minutes, type = 'pie',textposition = 'outside',textinfo = 'label+percent') %>%
  layout(title = 'Activity Level Minutes',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))



#clasified active users
active_users <- activity %>%
  filter(FairlyActiveMinutes >= 21.4 | VeryActiveMinutes>=10.7) %>% 
  group_by(Id) %>% 
  count(Id) 

#How active are the users
active_minute <- activity %>% 
  gather(key=Intensity, value=active_minutes, ends_with("minutes")) %>% 
  select(Intensity, active_minutes) 

#grafico de velas
ggplot(data=active_minute, aes(x=Intensity, y=active_minutes))+
  geom_boxplot(aes(fill=Intensity))+
  scale_x_discrete(limits=c("SedentaryMinutes","LightlyActiveMinutes","FairlyActiveMinutes","VeryActiveMinutes"))+
  ylab("Minutes")



# total step vs calories
ggplot(data=activity, aes(x=TotalSteps, y=Calories)) + 
  geom_point() + geom_smooth() + labs(title="Total Steps vs. Calories")



hourly_step$ActivityHour=as.POSIXct(hourly_step$ActivityHour,format="%m/%d/%Y %I:%M:%S %p")
hourly_step$Hour <-  format(hourly_step$ActivityHour,format= "%H")
head(hourly_step)


ggplot(data=hourly_step, aes(x=Hour, y=StepTotal, fill=Hour))+
  geom_bar(stat="identity")+
  labs(title="Hourly Steps")


ggplot(data=merged_data, aes(x=Weekday, y=TotalSteps, fill=Weekday))+ 
  geom_bar(stat="identity")+
  ylab("Total Steps")


hourly_step$ActivityHour=as.POSIXct(hourly_step$ActivityHour,format="%d/%m/%Y %I:%M:%S %p")
hourly_step$Hour <-  format(hourly_step$ActivityHour,format= "%H")
head(hourly_step)

hourly_step$ActivityHour <- as.POSIXct(hourly_step$ActivityHour, format="%m/%d/%Y %I:%M:%S%p")


rm(hourly_step)
hourly_step <- read.csv(hourly_steps_merged.csv)

ggplot(data=hourly_step, aes(x=Hour, y=StepTotal, fill=Hour))+
  geom_bar(stat="identity")+
  labs(title="Hourly Steps")

merged_data_hour <- merge(merged_data, hourly_step, by = c("Id"), all=TRUE)
write_csv(merged_data_hour, "merged_data_hour.csv")


# Interesting find here that some user who are sedentary, takes minimal step, but still able to burn over 1500 to 2500 calories
ggplot(data=activity, aes(x=TotalSteps, y = Calories, color=SedentaryMinutes))+ 
  geom_point()+ 
  labs(title="Total Steps vs Calories")+
  xlab("Total Steps")+
  stat_smooth(method=lm)+
  scale_color_gradient(low="orange", high="steelblue")


# Users who take more steps, burn more calories and has lower BMI. We also see some outliers in the top left corner. 
ggplot(data=merged_data, aes(x=TotalSteps, y = BMI, color=Calories))+ 
  geom_point()+ 
  stat_smooth(method=lm)+
  scale_color_gradient(low="blue", high="yellow")

#for now this is the end of the code 