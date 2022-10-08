# Bella Beat Case Study
A focus on producing a growth marketing strategy by using R language to clean and analyze data.  
## About The Company 
Bellabeat is a high tech company that manufactures smart health and fitness devices. It was founded by Urska Srsen and Sandon Mur and it has been widely successful with different products launches. The company has invested in traditonal advertising media, such as radio, out-of-home billboards, print, and television, but they are focused on digital marketing extensively. Bellabeat invests year-around in Google Search, maintaining active social media accounts and they also run ad campaigns across different platforms. 
## Business Task
Developing a new growth marketing strategy for the Bellabeat product by utilizing trends from smart fitness device users.
## Questions to Ask 
1. What are some trends in smart device usage?
2. How could these trends apply to Bellabeat customers?
3. How could these trends help influence Bellabeat marketing strategy?
## Loading Packages
``` {r} 
    library(tidyverse)
    library(tidyr)
    library(dplyr)
    library(lubridate)
    library(ggplot2) 
```
Checked data importation with glimpse() and View() function. 

Changed directory before next step through session, set working directory, choose directory, and chose a file. 

## Importing Data/Organizing Data
```{r} 
DailyActivity <- read.csv("~/Desktop/Fitabase Data 4.12.16-5.12.16 2/dailyActivity_merged.csv")
Calories <- read.csv("~/Desktop/Fitabase Data 4.12.16-5.12.16 2/hourlyCalories_merged.csv")
Intensities <- read.csv("~/Desktop/Fitabase Data 4.12.16-5.12.16 2/hourlyIntensities_merged.csv")
Sleep <- read.csv("~/Desktop/Fitabase Data 4.12.16-5.12.16 2/sleepDay_merged.csv")
Weight <- read.csv("~/Desktop/Fitabase Data 4.12.16-5.12.16 2/weightLogInfo_merged.csv")
``` 
## Cleaning Data
load the lubridate package Library(lubridate) 
```{r}
#intensities 

Intensities$ActivityHour <- parse_date_time(Intensities$ActivityHour, "%m/%d/%Y %I:%M:%S %p")
Intensities$date <- as.Date(Intensities$ActivityHour, format = "%d/%m/%Y")
Intensities$time <- format(as.POSIXct(Intensities$ActivityHour), format = "%H:%M:%S")

#Weight

Weight$Date <- parse_date_time(Weight$Date, "%m/%d/%Y %I:%M:%S %p")
Weight$date <- as.Date(Weight$Date, format = "%d/%m/%Y")
Weight$time <- format(as.POSIXct(Weight$Date), format = "%H:%M:%S")

#Sleep

Sleep$SleepDay <-parse_date_time(Sleep$SleepyDay, "%m/%d/%Y" %I:%M:%S %p")
Sleep$date <- as.Date(Sleep$SleepDay, format = "%d/%m/%Y")
Sleep$time <- format(as.POSIXct(Sleep$SleepDay), format = "%H:%M:%S")

#Calories

Calories$ActivityHour <- parse_date_time(Calories$ActivityHour, "%m/%d/%Y %I:%M%:%S")
Calories$date <- as.Date(Calories$ActivityHour, format = "%d/%m/%Y")
Calories$time <- format(as.POSIXct(Calories$ActivityHour), format = "%H:&M:%S")
```

## Number of participants
```{r}
n_distinct(DailyActivity$Id)
n_distinct(Weight$Id)
n_distinct(Sleep$Id)
n_distinct(Calories$Id)
n_distinct(Intensities$Id)
```
The maximum participants for each piece of data was 33 participants. Daily activity data had 33 participants, weight had 8 participants, sleep had 24 participants, calories had 33 participants, and intensities had 33 participants. Thus, making this data insufficient in making a strong conclusion however due to the limited information given on this topic in public data this will do for the moment. 

## Transforming Data
```{r}
#Daily Activity

DailyActivity%>%
  select(TotalSteps,TotalDistance,SedentaryMinutes,Calories)%>%
  summary()
   

#Calories

Calories%>%
  select(Calories)%>%
  summary()
    

#Sleep
  
Sleep%>%
  select(TotalMinutesAsleep,TotalTimeInBed)%>%
  summary()
  

#Weight

Weight%>%
  select(WeightPounds,BMI)%>%
  summary()
  
```
## Merging Two Data Sets
First a format needs to be changed in order for the merge to adequately change the format to create a new column called date.
```{r}
DailyActivity$date <- as.Date(DailyActivity$ActivityDate, format = "%m/%d/%Y")

sleep_vs_activity <- merge(Sleep, DailyActivity, by = c("Id","date"))
head(sleep_vs_activity)
```
## Visualization
```{r}
#Total Steps Vs. Calories

```{r}
#Total Steps Vs. Calories

ggplot(data=DailyActivity)+
    geom_point(mapping = aes(x=TotalSteps, y=Calories)) +
    geom_smooth(mapping = aes(x=TotalSteps, y=Calories)) +
    labs(title="TotalSteps Vs. Calories") +
    theme(plot.title = element_text(hjust=0.5))
```

A **positive trend** appears on the graph as the total steps that the individual took to reach their personal goals the more calories they burned. There are outliers on the graph but they could have been different reasons why they still lost many calories without taking any steps at all. For example, their are exercises that don't require taking steps like yoga or body building. Their also seems to be a cut off on the amount of calories that can be burned throughout the day which would be right under 4,000 calories. 

#Total Intensity Vs. Time
    
```{r}
ggplot(data=Intensities, aes(x=time, y=TotalIntensity))+
  geom_bar(stat = "identity", fill=('blue'))+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "Total Intensity vs. Time")+
  theme(plot.title = element_text(hjust=0.5))
```
The **highest usage rate** for the fitness device was during 12pm (noon) and 5pm to 7pm. It lets us know that the consumer focuses on physical activity either in between their work breaks and/or right after a 9-5 job. 
     
#Time Minutes Asleep Vs. Total Steps 
     
```{r}
ggplot(data=sleep_vs_activity)+
  geom_point(mapping = aes(x=TotalMinutesAsleep, y=SedentaryMinutes))+
  geom_smooth(mapping = aes(x=TotalMinutesAsleep, y=SedentaryMinutes))+
  labs(title = "Time Minutes Asleep vs. Sedentary Minutes")+
  theme(plot.title = element_text(hjust=0.5))
```
There is a **negative trend** happening on the graph meaning that the less active you are throughout the day the less time the body gets rest and proper sleep. The more time you are in a sedentary state the less you sleep. The difference in range is from the least active individual getting 50 minutes of sleep while those who had a lot of physical activity would sleep up to 13.33 hours. 

## Analysis

## Understanding the consumer 

The main type of consumer that needs to be targeted is the career driven individual with a 9-5 job. It was revealed through the intensity graph showing that the device usage rate increased after work hours. These individuals are focused, driven, disciplined and most likely go the gym after work to focus on their health.

## Strategies

1. In order to use the right customer acquisition strategy, first we need to understand the audience that needs to be targeted. This individual is health and fitness focused and would most likely purchase products to improve their lifestyles. One clear way to approach the consumer to a much more intimate level is to create partnerships with health and fitness influencers. Social Media is a place to share interests and its a thriving place to market to our consumers. Its appropriate to send off PR packages to influencers in order for them to share with their audience who engage in similar interests.This is a great way to expose the product to a niche audience that would most likely purchase the product. Early adopters will buy the device while others would consider making a decision in the near future. Its important to recognize that people buy from those who they trust on the subject or topic. 

2. Placing more features into the device like personal reminders to remind them to catch up with their goals. Provide food recommendations to improve fat burning and better sleep. Career focused individuals would appreciate notifications throughout the day to remind them to take a break, take a walk, or do a simple breathing exercise.This would help with their sleeping patterns which would then help improve focus and prevent burnout at work. Creating a campaign to promote the new features utilizing a spokesperson who has experience using the product features within their athletic profession. 

3. If there is a heavy competitive market within this segmentation it would be best to add some new technology that will increase market share. New ideas like tracking brain patterns or immune system activity to understand disease and its correlation towards active and sedentary lifestyles. This would be the most expensive strategy since it would take a lot of R&D to prepare. If the company is ready for this type of scaling then it could disrupt the market segmentation or the market as a whole. 
