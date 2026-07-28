# Title:    Forecasting Practices Exercises from Forecasting:Principles and Practice 3rd Edition
# File:     Forecasting_Practice 260707.R
# Project:  Forecasting_Practice

# CLEAN AND CLEAR THE ENVIRONMENT --------------------------
rm(list = ls())             ## Clear environment
# cat("\014")                 ## Clear console, ctrl+L

# INSTALL AND LOAD PACKAGES --------------------------------
pacman::p_load(magrittr, pacman, tidyverse,ggforce)
library(fpp3) # load the forecasting package
library(fable)
library(tidyverse)

# LOAD AND PREPARE DATA ------------------------------------
# pathway_summary <- # import the previously prepared pathway_summary csv file
#   read_csv("./Data/Student_Pathway_Summary_251114.csv", guess_max = 1000) %>% # guess_max ensures empty rows not treated as logical values 
#   as_tibble()

# y <- tsibble(
#   Year = 2015:2019,
#   Observation = c(123, 39, 78, 52, 110),
#   index = Year
# )
# melsyd_economy <- 
#   ansett %>% 
#   filter(Airports == 'MEL-SYD', Class == "Economy") %>% 
#   mutate(Passengers = Passengers/1000)
# 
# print(
# autoplot(melsyd_economy, Passengers) +
#   labs (title = "Ansett airlines economy class",
#         subtitle = "Melbourne-Sydney",
#         y = "Passengers ('000)")
# )

aus_economy <- global_economy |>
  filter(Code == "AUS") |>
  mutate(Pop = Population / 1e6)
autoplot(aus_economy, Pop) +
  labs(y = "Millions", title = "Australian population")

fit <- 
  aus_economy %>% 
  model(
    AAN = ETS(Pop ~ error("A") + trend("A") + season("N"))
  )

fc <-
  fit %>% 
  forecast(h=10)

# PRACTICE ENROLLMENT TIME SERIES ######
enrollment_data <- 
  read_csv("./Data/time_series_dhs 2026-07-26 practice.csv") %>% 
  as_tibble() %>%
  select(c(hs_code, starts_with("sem"))) %>% 
  select(c('hs_code','sem_5', 'sem_8', 'sem_11', 'sem_14',
           'sem_17', 'sem_20', 'sem_23', 'sem_26',
           'sem_29', 'sem_32')) %>% 
  # group_by(hs_code) %>% 
  select(!'hs_code') %>% 
  summarize(across(starts_with('sem'), ~sum(.x)))

# enrollment_time_series <- 
#   enrollment_data %>% 
#   t() %>% 
#   as_tibble() %>% 
#   setNames("enrollment") %>% 
#   as_tsibble()

enrollment_time_series <- 
  tibble(
  sem = (c(5,8,11,14,17,20,23,26,29,32)),
  enrollment = unlist(enrollment_data, use.names = FALSE))%>% 
  as_tsibble(index = sem)

new_students <- 
  enrollment_time_series

training_data <- 
  new_students %>% 
  slice(1:9)

autoplot(new_students, enrollment) +
  labs(y = "New Students", title = "semester code")

enroll_fit <- 
  training_data %>% 
  model(
    AAN = ETS(enrollment ~ error("A") + trend("Ad") + season("N"))
  )

enroll_fc <-
  enroll_fit %>% 
  forecast(h=2)

