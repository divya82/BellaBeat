install.packages("scales")
library(tidyverse)
library(janitor)
library(dplyr)
library(lubridate)
install.packages('jtools')
library("scales")

daily_activity <- read_csv("C:/Users/visha/OneDrive/Documents/Bellabeat/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
daily_Calories <- read_csv("OneDrive/Documents/Bellabeat/Fitabase Data 4.12.16-5.12.16/dailyCalories_merged.csv")
daily_Intensities <- read_csv("C:/Users/visha/OneDrive/Documents/Bellabeat/Fitabase Data 4.12.16-5.12.16/dailyIntensities_merged.csv")
daily_Steps <- read_csv("C:/Users/visha/OneDrive/Documents/Bellabeat/Fitabase Data 4.12.16-5.12.16/dailySteps_merged.csv")
heartrate_sec <- read_csv("C:/Users/visha/OneDrive/Documents/Bellabeat/Fitabase Data 4.12.16-5.12.16/heartrate_seconds_merged.csv")
sleep_Day <- read_csv("C:/Users/visha/OneDrive/Documents/Bellabeat/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
weightLogInfo <- read_csv("C:/Users/visha/OneDrive/Documents/Bellabeat/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")

Calories_Activity <- inner_join(daily_activity,daily_Calories,c("Id","Calories"))

daily <- inner_join(Calories_Activity,daily_Intensities,c("Id","ActivityDay",
      "SedentaryMinutes","LightlyActiveMinutes",
      "FairlyActiveMinutes","VeryActiveMinutes","SedentaryActiveDistance", 
      "LightActiveDistance", "ModeratelyActiveDistance", "VeryActiveDistance"))

daily_data <- inner_join(daily,sleep_Day,c("Id")) %>% drop_na() 


User_type <- daily_data %>% summarise(Users = factor(case_when(
  SedentaryMinutes > mean(SedentaryMinutes) & FairlyActiveMinutes < mean(FairlyActiveMinutes)
  & LightlyActiveMinutes < mean(LightlyActiveMinutes) & 
    VeryActiveMinutes < mean(VeryActiveMinutes) ~ "Sedentary",
  
  SedentaryMinutes < mean(SedentaryMinutes) & FairlyActiveMinutes < mean(FairlyActiveMinutes)
  & LightlyActiveMinutes > mean(LightlyActiveMinutes) & 
    VeryActiveMinutes < mean(VeryActiveMinutes) ~ "Lightly Active",
  
  SedentaryMinutes < mean(SedentaryMinutes) & FairlyActiveMinutes > mean(FairlyActiveMinutes)
  & LightlyActiveMinutes < mean(LightlyActiveMinutes) & 
    VeryActiveMinutes < mean(VeryActiveMinutes) ~ "Fairly Active",
  
  SedentaryMinutes < mean(SedentaryMinutes) & FairlyActiveMinutes < mean(FairlyActiveMinutes)
  & LightlyActiveMinutes < mean(LightlyActiveMinutes) & 
    VeryActiveMinutes > mean(VeryActiveMinutes) ~ "Very Active"),
  levels= c("Sedentary","Lightly Active","Fairly Active","Very Active")),
     Calories, Id= Id) %>% drop_na()

sleep_type <- daily_data %>% summarise(sleep = factor(case_when(
  TotalMinutesAsleep <= 360 ~ "Bad Sleep",
  TotalMinutesAsleep > 360 & TotalMinutesAsleep <= 480 
  ~ "Normal Sleep",
  TotalMinutesAsleep > 480 ~ "Over Sleep"),
  levels=c("Bad Sleep","Over Sleep","Normal Sleep")), sleep_time = 
    TotalMinutesAsleep,Id= Id)

User_sleep_type <- inner_join(User_type,sleep_type,c("Id")) 

User_type_weight <- inner_join(User_type,weightLogInfo,c("Id"))

User_step_weight <- inner_join(daily_Steps,User_type_weight,c("Id"))

User_step_weight <- User_step_weight %>% select(Users,StepTotal,WeightKg) %>% 
  group_by(Users) %>% summarise(Mean_Step = mean(StepTotal),Mean_Weight=mean(WeightKg),0)

User_Analysis <- User_sleep_type %>% select_all() %>% 
group_by(Users,sleep) %>% summarise(Total_Calories = sum(Calories)) %>% 
  mutate(Percentage_Calories= percent(Total_Calories/sum(Total_Calories)))
  

User_Analysis_1 <- User_sleep_type %>% select(Users,Calories) %>% 
  group_by(Users) %>% summarise(Total_Calories = quantile(Calories,prob=c(0.2,0.5,0.8)))

  G1 <- ggplot(data=User_Analysis) + aes(x= Users,y= Percentage_Calories , fill = sleep)+
     geom_bar(stat = "identity" ,position= "dodge")+ theme(axis.text.y = element_text(angle= 45),
              legend.position="bottom")+ labs(title="Different users and their sleep cycle")

   G2 <- ggplot(data=User_Analysis_1) + aes(x= Users,y= Total_Calories, fill=Users)+
     geom_boxplot()+ labs(title="Calories burned by different Users")