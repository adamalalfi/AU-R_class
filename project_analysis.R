library(dplyr)
library(readr)
library(lubridate)
library(tidyr)
arrests <- read.csv("data/2024_dc_arrests.xlsx")


#racial breakdown. which race is the most arrested
# first I will group the data by race and then calculate the percentage
# of each race

race <- arrests |>
  group_by(Defendant.Race) |>
  summarize(total = n()) |>
  mutate(percent = round(total / sum(total) * 100, 1)) |>
  arrange(desc(total))
View(race)
#result: a signiificant diffrence btewwn black and white arrests with 83.7%
#of the arrests are black compared to over 10% white.

# gender breakdown what is the gender gap between arrestees
# to do that we will use the same process to determine the numbers and percentage for each group

gender<- arrests |>
  group_by(Defendant.Sex) |>
  summarise(total= n()) |>
  mutate(percent = round(total / sum(total) * 100, 1)) |>
  arrange(desc(total))
View(gender)

# findings male arrestees are almost triple the size of women 75.5% to 24.5%

# now I will explore the age demographics to do that I will count and rank ages to find the most frequent age range

age<- arrests |>
  mutate(Age=as.numeric(Age)) |>
  filter(!is.na(Age)) |>
  mutate(age_range = case_when(
    Age >= 18 & Age <= 20 ~ "18-20",
    Age >= 21 & Age <= 40 ~ "21-40",
    Age >= 41 & Age <= 49 ~ "41-49",
    Age >= 50 & Age <= 59 ~ "50-59",
    Age >= 60 ~ "60+"
  )) |>
  group_by(age_range) |>
  summarise(total = n()) |>
  mutate(percent = round(total / sum(total) * 100, 1)) |>
  arrange(desc(total))
View(age)
#findings: 65% of arrestees are btween the age of 21- 40
#with a mean age of:
mean(as.numeric(arrests$Age), na.rm = TRUE)
# 35.61964

#now I'd like to explore what is the primary offence the police is called for to make an adukt arrest
#ranking and arranging arrests reasons will detrmine that
reason<-arrests |>
  group_by(Arrest.Category) |>
  summarise(total=n()) |>
  mutate(percent = round(total / sum(total) * 100, 1)) |>
  arrange(desc(total))
View(reason)
#findings simple assaults and traffic violations are the most offenses in adultds arrests
# since the simple assult is the most reason I would like to explore the race percentage of each assult
# now I will create a pivot table to filter the perfcentage of each race for each offence
offence_race<- arrests |>
  filter(Defendant.Race %in% c("BLACK", "WHITE")) |>
  group_by(Arrest.Category, Defendant.Race) |>
  summarise(total = n()) |>
  mutate(percent = round(total / sum(total) * 100, 1)) |>
  mutate(overall_total = sum(total)) |>
  select(Defendant.Race, Arrest.Category, percent, total, overall_total ) |>
  pivot_wider(names_from = Defendant.Race,
              values_from = c(total, percent)) |>
  arrange(desc(overall_total))
View(offence_race)

#Data Viz.
# I will create a viz for the top 5 offences so I will slice them from the offence race table
#now I will use pivot longer to move black and white into one column called percentage and a new column called race
#after tat I will use geocol to place both races percentage side by side in the chart
library(ggplot2)
chart_data<- offence_race |>
  ungroup() |>
  slice_max(overall_total,n=5) |>
  select(Arrest.Category, percent_BLACK,percent_WHITE) |>
  pivot_longer(cols = starts_with("percent"),
               names_to = "race",
               values_to = "percent")

ggplot(chart_data, aes(x = reorder(Arrest.Category, percent), y = percent, fill = race)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_fill_manual(values = c("percent_BLACK" = "blue", "percent_WHITE" = "green"),
                    labels = c("Black", "White")) +
  labs(title = "Racial Comparison: Top 5 Arrest Categories",
       x = "Offense Category",
       y = "Percentage of Total",
       caption = "Data: 2024 MPD Arrests")



