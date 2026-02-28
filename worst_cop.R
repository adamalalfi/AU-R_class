library(dplyr)
library(readr)
library(lubridate)
library(stringr)
transponders <- read.csv("data/transponders.csv")
distances <- read.csv("data/distances.csv")
cop_data<- transponders |>
  left_join(distances, by =c("from_place", "to_place"))
cop_data<- cop_data |>
  mutate(start=mdy_hms(str_c(start_date, start_time, sep=" ")),
         end= mdy_hms(str_c(end_date, end_time, sep=" ")),
         hours= as.numeric(end- start , units = "hours"),
         speed = distance/ hours) |>
  filter(hours > 0)
View(cop_data)

#Q1 The highest speed reached any individual police vehicle

max(cop_data$speed, na.rm = TRUE)
#answer:  125.47 MPH

#Q2 The number of unique transponders in the data.
n_distinct(cop_data$transponder)
#answer: 73

#Q3 The highest average speed associated with a police vehicle/transponder.
cop_data |>
  group_by(transponder) |>
  summarise(avg_speed= mean(speed,na.rm= TRUE)) |>
  arrange(desc(avg_speed)) |>
  head(1)
#answer : 57483550110      89.2MPH

#Q4 Which toll start and end place has the highest average cop speed.

cop_data |>
  group_by(from_place, to_place) |>
  summarise(avg_speed= mean(speed, na.rm= TRUE)) |>
  arrange(desc(avg_speed)) |>
  head(1)
#answer:  Commercial - SunPass Only Ramp Griffin Rd East/West      93.0MPH



