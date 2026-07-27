# Title:    Fable Package Practice
# File:     Fable_Practice 260727.R
# Project:  Forecasting_Practice

# CLEAN AND CLEAR THE ENVIRONMENT --------------------------
rm(list = ls())             ## Clear environment
# cat("\014")                 ## Clear console, ctrl+L

# INSTALL AND LOAD PACKAGES --------------------------------
pacman::p_load(magrittr, pacman, tidyverse,ggforce)
library(fpp3) # load the forecasting package

# LOAD AND PREPARE DATA ------------------------------------

library(fable)
library(tsibble)
library(tsibbledata)
# library(lubridate)
# library(dplyr)

forecast <- 
aus_retail %>%
  filter(
    State %in% c("New South Wales", "Victoria"),
    Industry == "Department stores"
  ) %>% 
  model(
    ets = ETS(box_cox(Turnover, 0.3)),
    arima = ARIMA(log(Turnover)),
    snaive = SNAIVE(Turnover)
  ) %>%
  forecast(h = "2 years") %>% 
  autoplot(filter(aus_retail, year(Month) > 2010), level = NULL)

forecast %>% 
  fitted()
