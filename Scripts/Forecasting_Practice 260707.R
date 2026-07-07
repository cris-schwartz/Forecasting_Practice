# Title:    Forecasting Practices Exercises from Forecasting:Principles and Practice 3rd Edition
# File:     Forecasting_Practice 260707.R
# Project:  Forecasting_Practice

# CLEAN AND CLEAR THE ENVIRONMENT --------------------------
rm(list = ls())             ## Clear environment
# cat("\014")                 ## Clear console, ctrl+L

# INSTALL AND LOAD PACKAGES --------------------------------
pacman::p_load(magrittr, pacman, tidyverse,ggforce)
library(fpp3) # load the forecasting package

# LOAD AND PREPARE DATA ------------------------------------
# pathway_summary <- # import the previously prepared pathway_summary csv file
#   read_csv("./Data/Student_Pathway_Summary_251114.csv", guess_max = 1000) %>% # guess_max ensures empty rows not treated as logical values 
#   as_tibble()

# y <- tsibble(
#   Year = 2015:2019,
#   Observation = c(123, 39, 78, 52, 110),
#   index = Year
# )
melsyd_economy <- 
  ansett %>% 
  filter(Airports == 'MEL-SYD', Class == "Economy") %>% 
  mutate(Passengers = Passengers/1000)

print(
autoplot(melsyd_economy, Passengers) +
  labs (title = "Ansett airlines economy class",
        subtitle = "Melbourne-Sydney",
        y = "Passengers ('000)")
)