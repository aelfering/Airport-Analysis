#### LOADING THE LIBRARIES ####
library(lubridate)
library(dplyr)
library(tidyr)
library(tidyverse)
library(scales)
library(data.table)
library(zoo)
library(stringr)

setwd("~/Desktop")

options(scipen = 999)

# What are the largest airports based on total enplanements?
# per the T100 Market Segment report

market_t100 <- read.csv('34204039_T_T100D_MARKET_US_CARRIER_ONLY.csv')

sum_market <- market_t100 %>%
  group_by(ORIGIN) %>%
  summarise(PAX = sum(PASSENGERS)) %>%
  ungroup() %>%
  arrange(desc(PAX)) %>%
  filter(row_number() <= 50) %>%
  mutate(row = row_number())

on_time <- list.files("/Users/alexelfering/Desktop/On Time Data", pattern = "*.csv", full.names = TRUE)
on_time_df <- rbindlist(lapply(on_time, fread))

rolling_sum_int <- 7

### Stats Overall ####
all_stats <- on_time_df %>%
  replace(is.na(.), 0) %>%
  group_by(FL_DATE) %>%
  summarise(FLIGHTS = sum(FLIGHTS),
            DELAYS = sum(DEP_DEL15),
            CANCELLED = sum(CANCELLED),
            DIVERTED = sum(DIVERTED)) %>%
  ungroup() %>%
  mutate(OP_FLIGHTS = FLIGHTS-CANCELLED) %>%
  mutate(ROLLING_OP_FLIGHTS = rollapplyr(OP_FLIGHTS, rolling_sum_int, sum, partial = TRUE),
         ROLLING_FLIGHTS = rollapplyr(FLIGHTS, rolling_sum_int, sum, partial = TRUE),
         ROLLING_DELAYS = rollapplyr(DELAYS, rolling_sum_int, sum, partial = TRUE),
         ROLLING_CANCELLED = rollapplyr(CANCELLED, rolling_sum_int, sum, partial = TRUE),
         ROLLING_DEL_PCT = ROLLING_DELAYS/ROLLING_FLIGHTS) %>%
  mutate(ROLLING_PCT_CANCELLED = 1-(ROLLING_OP_FLIGHTS/ROLLING_FLIGHTS),
         ROLLING_PCT_DELAYED = 1-(ROLLING_DELAYS/ROLLING_OP_FLIGHTS)) %>%
  select(FL_DATE,
         ROLLING_FLIGHTS,
         ROLLING_OP_FLIGHTS,
         ROLLING_DELAYS,
         ROLLING_CANCELLED,
         ROLLING_DEL_PCT,
         ROLLING_PCT_DELAYED,
         ROLLING_CAN_PCT,
         ROLLING_PCT_CANCELLED)

min_fl_date <- as.Date(min(on_time_df$FL_DATE))
max_fl_date <- as.Date(max(on_time_df$FL_DATE))

# The primary cause of flight cancellation
on_time_df %>%
  replace(is.na(.), 0) %>%
  mutate(FL_DATE = as.Date(FL_DATE)) %>%
  filter(CANCELLED == 1) %>%
  group_by(FL_DATE,
           CANCELLATION_CODE) %>%
  summarise(CANCELLED = sum(CANCELLED)) %>%
  ungroup() %>%
  group_by(CANCELLATION_CODE) %>%
  complete(CANCELLATION_CODE, FL_DATE = seq.Date(min_fl_date, max_fl_date, by = 'day')) %>%
  replace(is.na(.), 0) %>%
  mutate(ROLLING_CANCELLED = rollapplyr(CANCELLED, rolling_sum_int, sum, partial = TRUE)) %>%
  ungroup() %>%
  group_by(FL_DATE) %>%
  mutate(ROLLING_TOTAL_CANCELLED = sum(ROLLING_CANCELLED)) %>%
  ungroup() %>%
  mutate(PCT_CANCELLED = ROLLING_CANCELLED/ROLLING_TOTAL_CANCELLED) %>%
  ggplot(aes(x = FL_DATE,
             y = PCT_CANCELLED,
             group = CANCELLATION_CODE,
             color = CANCELLATION_CODE)) +
  scale_y_continuous(labels = percent) +
  scale_color_manual(labels = c("Carrier", 
                                "Weather", 
                                'National Air System', 
                                'Security'), 
                     values = c("#F0E918", 
                                "#24ABF0", 
                                '#7624F0', 
                                '#F08500')) +
  labs(title = 'The Primary Cause for Cancellations among Domestic Flights',
       subtitle = paste('Based on a rolling ', rolling_sum_int, ' day sum', sep = ''),
       captions = 'Visualization by Alex Elfering\nSource: Bureau of Transportation Statistics',
       x = '',
       y = '',
       color = 'Legend: ') +
  geom_vline(xintercept = as.Date('2020-03-12')) +
  geom_line(size = 1) +
  theme(plot.title = element_text(face = 'bold', size = 18, family = 'Arial'),
        legend.position = 'top',
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.text = element_text(size = 12, family = 'Arial'),
        legend.title = element_text(size = 12, family = 'Arial'),
        plot.subtitle = element_text(size = 15, family = 'Arial'),
        plot.caption = element_text(size = 12, family = 'Arial'),
        axis.title = element_text(size = 12, family = 'Arial'),
        axis.text = element_text(size = 12, family = 'Arial'),
        strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'black', family = 'Arial'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.y = element_line(colour = "gray", linetype = "dashed"),
        panel.grid.major.x = element_blank()) 

ggplot(all_stats,
       aes(x = as.Date(FL_DATE),
           y = ROLLING_PCT_CANCELLED)) +
  scale_y_continuous(labels = percent) +
  geom_line(size = 1) +
  labs(title = 'Percent of Flights Cancelled',
       subtitle = paste('Based on a rolling ', rolling_sum_int, ' day sum', sep = ''),
       x = '',
       y = '') +
  theme(plot.title = element_text(face = 'bold', size = 18, family = 'Arial'),
        legend.position = 'top',
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.text = element_text(size = 12, family = 'Arial'),
        legend.title = element_text(size = 12, family = 'Arial'),
        plot.subtitle = element_text(size = 15, family = 'Arial'),
        plot.caption = element_text(size = 12, family = 'Arial'),
        axis.title = element_text(size = 12, family = 'Arial'),
        axis.text = element_text(size = 12, family = 'Arial'),
        strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'black', family = 'Arial'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.y = element_line(colour = "gray", linetype = "dashed"),
        panel.grid.major.x = element_blank()) 

ggplot(all_stats,
       aes(x = as.Date(FL_DATE),
           y = ROLLING_OP_FLIGHTS)) +
  geom_line(size = 1,
            color = '#E6B60B') +
  geom_line(mapping = aes(x = as.Date(FL_DATE),
                          y = ROLLING_CANCELLED),
            size = 1,
            color = '#59E60B') +
  labs(title = 'Cancellations are down, but so are Flights',
       subtitle = paste('Based on a rolling ', rolling_sum_int, ' day sum', sep = ''),
       x = '',
       y = '') +
  theme(plot.title = element_text(face = 'bold', size = 18, family = 'Arial'),
        legend.position = 'top',
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.text = element_text(size = 12, family = 'Arial'),
        legend.title = element_text(size = 12, family = 'Arial'),
        plot.subtitle = element_text(size = 15, family = 'Arial'),
        plot.caption = element_text(size = 12, family = 'Arial'),
        axis.title = element_text(size = 12, family = 'Arial'),
        axis.text = element_text(size = 12, family = 'Arial'),
        strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'black', family = 'Arial'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.y = element_line(colour = "gray", linetype = "dashed"),
        panel.grid.major.x = element_blank()) 

####  By Airlines ####

on_time_df %>%
  replace(is.na(.), 0) %>%
  group_by(FL_DATE,
           MKT_CARRIER) %>%
  summarise(FLIGHTS = sum(FLIGHTS),
            DELAYS = sum(DEP_DEL15),
            CANCELLED = sum(CANCELLED),
            DIVERTED = sum(DIVERTED)) %>%
  ungroup() %>%
  group_by(MKT_CARRIER) %>%
  mutate(OP_FLIGHTS = FLIGHTS-CANCELLED) %>%
  mutate(CARRIER_ROLLING_OP_FLIGHTS = rollapplyr(OP_FLIGHTS, rolling_sum_int, sum, partial = TRUE),
         CARRIER_ROLLING_FLIGHTS = rollapplyr(FLIGHTS, rolling_sum_int, sum, partial = TRUE),
         CARRIER_ROLLING_DELAYS = rollapplyr(DELAYS, rolling_sum_int, sum, partial = TRUE),
         CARRIER_ROLLING_CANCELLED = rollapplyr(CANCELLED, rolling_sum_int, sum, partial = TRUE),
         FL_DATE = as.Date(FL_DATE)) %>%
  ungroup() %>%
  mutate(CARRIER_PCT_DELAYED = CARRIER_ROLLING_DELAYS/CARRIER_ROLLING_FLIGHTS,
         CARRIER_PCT_CANCELLED = 1-(CARRIER_ROLLING_OP_FLIGHTS/CARRIER_ROLLING_FLIGHTS) ) %>%
  
  

  
  ggplot(aes(x = FL_DATE,
             y = CARRIER_PCT_CANCELLED,
             group = MKT_CARRIER)) +
  scale_y_continuous(labels = percent) +
  labs(title = 'Cancellation Rates among U.S. Market Carriers vs. the Nation',
       subtitle = paste('Based on a rolling ', rolling_sum_int, ' day sum', sep = ''),
       color = 'Airline') +
  #geom_line(mapping = aes(x = FL_DATE,
  #                        y = ALL_ROLLING_CAN_PCT,
  #                        group = MKT_CARRIER),
  #          color = 'black',
  #          size = 1) + 
  geom_line(size = 1,
            color = '#E66800') + 
  facet_wrap(~MKT_CARRIER) +
  theme(plot.title = element_text(face = 'bold', size = 18, family = 'Arial'),
        legend.position = 'top',
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.text = element_text(size = 12, family = 'Arial'),
        legend.title = element_text(size = 12, family = 'Arial'),
        plot.subtitle = element_text(size = 15, family = 'Arial'),
        plot.caption = element_text(size = 12, family = 'Arial'),
        axis.title = element_text(size = 12, family = 'Arial'),
        axis.text = element_text(size = 12, family = 'Arial'),
        strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'black', family = 'Arial'),
        strip.background = element_rect(fill = NA),
        panel.background = element_rect(fill = NA, color = "black"),
        #panel.border = element_rect(colour = "black"),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.y = element_line(colour = "gray", linetype = "dashed"),
        panel.grid.major.x = element_blank()) 

MKT_AIRPORT <- on_time_df %>%
  replace(is.na(.), 0) %>%
  group_by(MKT_CARRIER, 
           ORIGIN) %>%
  summarise(FLIGHTS = sum(FLIGHTS),
            DELAYS = sum(DEP_DEL15),
            CANCELLED = sum(CANCELLED),
            DIVERTED = sum(DIVERTED)) %>%
  ungroup() %>%
  arrange(MKT_CARRIER,
          desc(FLIGHTS)) %>%
  group_by(MKT_CARRIER) %>%
  mutate(RANK = dense_rank(desc(FLIGHTS))) %>%
  ungroup() %>%
  filter(RANK <= 20) %>%
  mutate(PCT_DEL = DELAYS/FLIGHTS,
         PCT_CAN = 1-((FLIGHTS-CANCELLED)/FLIGHTS) )

recent_mkt_airport_data <- on_time_df %>%
  replace(is.na(.), 0) %>%
  filter(FL_DATE >= max_fl_date-14) %>%
  group_by(MKT_CARRIER, 
           ORIGIN) %>%
  summarise(FLIGHTS = sum(FLIGHTS),
            DELAYS = sum(DEP_DEL15),
            CANCELLED = sum(CANCELLED),
            DIVERTED = sum(DIVERTED)) %>%
  ungroup() %>%
  arrange(MKT_CARRIER,
          desc(FLIGHTS)) %>%
  group_by(MKT_CARRIER) %>%
  ungroup() %>%
  mutate(PCT_DEL = DELAYS/FLIGHTS,
         PCT_CAN = CANCELLED/FLIGHTS)

compare_last_two_weeks <- MKT_AIRPORT %>%
  left_join(recent_mkt_airport_data,
            by = c('MKT_CARRIER' = 'MKT_CARRIER',
                   'ORIGIN' = 'ORIGIN')) %>%
  select(MKT_CARRIER,
         RANK,
         ORIGIN,
         Overall_Delay_Pct = PCT_DEL.x,
         Overall_Cancellation_Pct = PCT_CAN.x,
         Two_Week_Delay_Pct = PCT_DEL.y,
         Two_Week_Cancellation_Pct = PCT_CAN.y) %>%
  mutate(Can_Diff = Two_Week_Cancellation_Pct-Overall_Cancellation_Pct,
         Delay_Diff = Two_Week_Delay_Pct-Overall_Delay_Pct)

mark1 <- on_time_df %>%
  replace(is.na(.), 0) %>%
  group_by(MKT_CARRIER, 
           ORIGIN,
           DEST) %>%
  summarise(FLIGHTS = sum(FLIGHTS),
            DELAYS = sum(DEP_DEL15),
            CANCELLED = sum(CANCELLED),
            DIVERTED = sum(DIVERTED)) %>%
  ungroup() %>%
  arrange(MKT_CARRIER,
          desc(FLIGHTS)) %>%
  group_by(MKT_CARRIER) %>%
  mutate(RANK = dense_rank(desc(FLIGHTS))) %>%
  ungroup() %>%
  mutate(PCT_DEL = DELAYS/FLIGHTS,
         PCT_CAN = (1-(FLIGHTS-CANCELLED) /FLIGHTS))

####  By Airports ####

origin_stats <- on_time_df %>%
  filter(ORIGIN %in% sum_market$ORIGIN) %>%
  replace(is.na(.), 0) %>%
  group_by(FL_DATE,
           ORIGIN,
           ORIGIN_CITY_NAME) %>%
  summarise(FLIGHTS = sum(FLIGHTS),
            DELAYS = sum(DEP_DEL15),
            CANCELLED = sum(CANCELLED),
            DIVERTED = sum(DIVERTED)) %>%
  ungroup() %>%
  mutate(ROLLING_FLIGHTS = rollapplyr(FLIGHTS, rolling_sum_int, sum, partial = TRUE),
         ROLLING_DELAYS = rollapplyr(DELAYS, rolling_sum_int, sum, partial = TRUE),
         ROLLING_CANCELLED = rollapplyr(CANCELLED, rolling_sum_int, sum, partial = TRUE),
         ROLLING_DIVERTED = rollapplyr(DIVERTED, rolling_sum_int, sum, partial = TRUE),
         ROLLING_DEL_PCT = ROLLING_DELAYS/ROLLING_FLIGHTS,
         ROLLING_CAN_PCT = ROLLING_CANCELLED/ROLLING_FLIGHTS,
         ROLLING_DIV_PCT = ROLLING_DIVERTED/ROLLING_FLIGHTS,
         FL_DATE = as.Date(FL_DATE)) %>%
  select(FL_DATE,
         ORIGIN,
         ORIGIN_CITY_NAME,
         ROLLING_DEL_PCT,
         ROLLING_CAN_PCT,
         ROLLING_DIV_PCT)

compare_against_all <- origin_stats %>%
  inner_join(all_stats, by = c('FL_DATE' = 'FL_DATE')) %>%
  select(FL_DATE,
         ORIGIN,
         ORIGIN_CITY_NAME,
         ROLLING_DEL_PCT,
         ROLLING_CAN_PCT,
         ROLLING_DIV_PCT,
         ALL_ROLLING_DEL_PCT,
         ALL_ROLLING_CAN_PCT,
         ALL_ROLLING_DIV_PCT) %>%
  mutate(CAN_DIFF = ROLLING_CAN_PCT-ALL_ROLLING_CAN_PCT) %>%
  unite('ORIGIN_AIRPORT', c('ORIGIN', 'ORIGIN_CITY_NAME'), sep = '-', remove = TRUE, na.rm = TRUE)

date <- as.Date('2020-03-20')

inc_can_airports <- compare_against_all %>%
  filter(FL_DATE == date,
         CAN_DIFF > 0) %>%
  arrange(desc(CAN_DIFF)) %>%
  select(ORIGIN_AIRPORT,
         CAN_DIFF) %>%
  arrange(desc(CAN_DIFF))

inc_can_airports$ORIGIN_AIRPORT <- factor(inc_can_airports$ORIGIN_AIRPORT, 
                                          levels = inc_can_airports$ORIGIN_AIRPORT[order(inc_can_airports$CAN_DIFF)], ordered=TRUE)

ggplot(subset(compare_against_all, ORIGIN_AIRPORT %in% inc_can_airports$ORIGIN_AIRPORT),
       aes(x = FL_DATE,
           y = ROLLING_CAN_PCT)) +
  geom_vline(xintercept = date,
             linetype = 'dashed',
             color = 'gray') +
  geom_line(color = 'orange',
            size = 1) +
  geom_line(mapping = aes(x = FL_DATE,
                          y = ALL_ROLLING_CAN_PCT),
            size = 1) +
  facet_wrap(~ORIGIN_AIRPORT)

