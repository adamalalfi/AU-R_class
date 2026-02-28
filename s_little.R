library(dplyr)
library(tidyr)
source("Data/03_import_murders.R")
#case: finding victims for Samuel Little
#https://www.murderdata.org/2018/12/an-analysis-of-many-murders-of-samuel.html
#https://www.biography.com/crime/samuel-little
library(readr)
county.fips <- read.csv("data/fips_counties (1).csv")

murders  <- murders|>
  mutate(CNTYFIPS=as.numeric(as.character(CNTYFIPS)))|>
  mutate(CNTYFIPS=case_when(
    CNTYFIPS==51560 ~ 51005,
    CNTYFIPS==2232 ~ 2105,
    CNTYFIPS==2280 ~ 2195,
    CNTYFIPS==2201 ~ 2198,
    TRUE ~ CNTYFIPS
  ))|>
  left_join(county.fips, by=c("CNTYFIPS"="fips"))
# filter the LA murders during peatrk years
# keywords: LA County, female, 1988-1999, strangulation or unknown weapon
little_la_murders<- murders |>
  filter(state_abbrev == "CA" & name_of_county == "Los Angeles") |>
  filter(Year %in% 1980:1999) |>
  filter(VicSex_label == "Female") |>
  filter(Weapon_label == "Strangulation - hanging" |
           Weapon_label == "Other or type unknown")
View(little_la_murders)

little_summary_table <- little_la_murders |>
  mutate(solved_num = ifelse(Solved_label == "Yes", 1, 0)) |>
  group_by(name_of_county, Weapon_label) |>
  summarize(
    total_cases = n(),
    solved_cases = sum(solved_num)
  ) |>
  mutate(clearance_rate = round(solved_cases / total_cases * 100, 2))
  View(little_summary_table)
