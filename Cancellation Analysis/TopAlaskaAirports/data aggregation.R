library(ggplot2)  
library(shiny)
library(htmltools)
library(shinydashboard)
library(shinythemes) 
library(dplyr) 
library(lubridate)
library(tidyr)
library(tidyverse)
library(scales)
library(data.table)
library(zoo)
library(ggrepel)
library(rsconnect)

on_time <- list.files("~/Desktop/On Time Data", pattern = "*.csv", full.names = TRUE)
on_time_df <- rbindlist(lapply(on_time, fread))

top_ak_airports <- on_time_df %>%
  replace(is.na(.), 0) %>%
  filter(ORIGIN_STATE_NM == 'Alaska',
         FL_DATE >= '2020-01-01') %>%
  group_by(ORIGIN) %>%
  summarise(FLIGHTS = sum(FLIGHTS)) %>%
  ungroup() %>%
  filter(dense_rank(desc(FLIGHTS)) <= 5)

all_stats <- on_time_df %>%
  replace(is.na(.), 0) %>%
  filter(ORIGIN %in% top_ak_airports$ORIGIN) %>%
  group_by(FL_DATE,
           ORIGIN) %>%
  summarise(FLIGHTS = sum(FLIGHTS),
            DELAYS = sum(DEP_DEL15),
            CANCELLED = sum(CANCELLED),
            DIVERTED = sum(DIVERTED)) %>%
  ungroup()

write.csv(all_stats, 'top_alaska_airport_flights.csv')

