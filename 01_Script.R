library(readr)
library(dplyr)
install.packages("readxl")
library(readxl)
#imort data
#filtered DC
ff<-read_csv("Data/fatal-police-shootings-data.csv")
dc<-ff|>
  filter(state=="DC")
#export data DC only
write_csv(dc,"Data/dc_shooting.csv")

#import XL file

#clean data
mlb<-read_excel("Data/MLBpayrolls.xlsx", sheet=4, skip=4)
install.packages("janitor")
library(janitor)

