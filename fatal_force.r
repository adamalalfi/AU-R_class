library(readr)
library(dplyr)
library(lubridate)
#importing file
ff <- read.csv("data/fatal-police-shootings-data - fatal-police-shootings-data (12).csv")
#Q! What is the average age of shooting victims for each year?
#creating a new table for Q1
#Select the age and date columns and then use mutate to create a year column from the date
#use group by to group rows by each year
#use summaries to calculate the mean age and skip the blank values
q1<-ff |>
  select(date,age) |>
  mutate(year= year(date)) |>
  group_by(year)|>
  summarise(average_age= mean(age, na.rm= TRUE))

#Q2 Which race is killed most by police?
#new table
#and then select the race column an group the data by race
#after that I used summarize to get the total count of each race and then sort them from high to low
q2<-ff |>
  select(race) |>
  group_by(race) |>
  summarise(total_killings = n()) |>
  arrange(desc(total_killings))

#Q3 What percent mental illness played a role the incidents
#@create new table and them select and group mental health column and then summarize to calculate the total and mutate to calculate the percent
q3<-ff |>
  select(was_mental_illness_related) |>
  group_by(was_mental_illness_related) |>
  summarise(total = n() ) |>
  mutate(percent = (total / sum(total)) * 100)

# Q4 What percent of the killings were captured by body cameras
q4<-ff |>
  select(body_camera) |>
  summarise(total= n(), body_cam_cases = sum(body_camera ==TRUE, na.rm = TRUE)) |>
  mutate(percent = (body_cam_cases / total) * 100)











