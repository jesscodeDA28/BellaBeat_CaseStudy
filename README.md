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
DailyActivity <- read.csv("dailyActivity_merged.csv")
Calories <- read.csv("hourlyCalories_merged.csv")
Intensities <- read.csv("hourlyIntensities_merged.csv")
Sleep <- read.csv("sleepDay_merged.csv")
Weight <- read.csv("weightloginfo_merged.csv")
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
n_distinct(Weight$Id
n_distinct(Sleep$Id)
n_distinct(Calories$Id)
n_distinct(Intensities$Id)
```
The maximum participants for each piece of data was 33 participants. Daily activity data had 33 participants, weight had 8 participants, sleep had 24 participants, calories had 33 participants, and intensities had 33 participants. Thus, making this data insufficient in making a strong conclusion however due to the limited information given on this topic in public data this will do for the moment. 

## Transforming Data
```{r}
#Daily Activity

DailyActivity %>%
     Select(TotalSteps,TotalDistance,SedentaryMinutes,Calories) %>%
     Summary()
     
#Calories

Calories %>%
    Select(Calories)%>%
    Summary()

#Sleep
  
Sleep %>%
    Select(TotalMinutesAsleep,TotalTimeInBed)%>%
    Summary()
    
#Weight

Weight %>%
  Select(WeightPounds,BMI)%>%
  Summary()
```
## Merging Two Data Sets
First a format needs to be changed in order for the merge to adequately change the format to create a new column called date.
```{r}
DailyActivity$date <- as.Date(DailyActivity$ActivityDate, format = "%m/%d/%Y")

sleep_vs_activity <- merge(Sleep, DailyActivity, by = c("Id","date"))
```
## Visualization
```{r}
#Total Steps Vs. Calories

ggplot(data=DailyActivity)+
    geom_point(mapping = aes(x=TotalSteps, y=Calories))+
    geom_smooth(mapping = aes(x=TotalSteps, y=Calories))+
    labs(title="TotalSteps Vs. Calories")+
    theme(plot.title = element_text(hjust=0.5)
    
#Total Intensity Vs. Time
    
ggplot(data=Intensities, aes(x=time, y=TotalIntensity))+
     geom_bar(stat= "Identity", fill =('blue'))+
     theme(axis.text.x = element_text(angle = 90))+
     labs(title = "Total Intensity vs. Time")+
     theme(plot.title = element_text(hjust=0.5)
     
#Time Minutes Asleep Vs. Total Steps 
     
ggplot(data=sleep_vs_activity)+
     geom_point(mapping = aes(x=TotalMinutesAsleep, y=SedentaryMinutes))+
     geom_smooth(mapping = aes(x=TotalMinutesAsleep, y=SedentaryMinutes))+
     labs(title= "Time Minutes Asleep vs. Total Steps")+
     theme(plot.title = element_text(hjust=0.5))
```

