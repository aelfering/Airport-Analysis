library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)
library(zoo)
library(ggrepel)

setwd("~/Documents/GitHub/Airport-Analysis/Covid 19 Analysis")

####  Ranking Airports Based on 2019 Market Data  ####
t100_19 <- read.csv('T100 2019.csv')

head(t100_19)

airport_pax <- t100_19 %>%
  group_by(ORIGIN) %>%
  summarise(PAX = sum(PASSENGERS)) %>%
  ungroup() %>%
  arrange(desc(PAX)) %>%
  filter(row_number() <= 100)

####  On-Time Data ####
airport_files <- list.files("~/Documents/GitHub/Airport-Analysis/Covid 19 Analysis/Airport Data", pattern = "*.csv", full.names = TRUE)

airport_df <- as.data.frame(rbindlist(lapply(airport_files, fread)))

head(airport_df)

airport_df %>%
  mutate(FL_DATE = ymd(FL_DATE)) %>%
  filter(FL_DATE >= '2020-03-01') %>%
  distinct(FL_DATE) %>%
  arrange(FL_DATE) %>%
  mutate(weeks = ifelse(FL_DATE >= max(FL_DATE)-14, 1, 0),
         weeks = ifelse(FL_DATE < max(FL_DATE)-14 & FL_DATE >= max(FL_DATE)-28, 2, weeks))



mark1 <- airport_df %>%
  # Format dates correctly and control to look at March and April
  mutate(FL_DATE = ymd(FL_DATE)) %>%
  filter(FL_DATE >= '2020-03-01',
         ORIGIN %in% airport_pax$ORIGIN) %>%
  # Replace all NA values with 0 for good measure
  replace(is.na(.), 0) %>%
  # Summarise total flights and cancelled flights by airport
  group_by(FL_DATE,
           ORIGIN) %>%
  mutate(FLIGHTS = sum(FLIGHTS),
         CANCELLED = sum(CANCELLED)) %>%
  ungroup() %>%
  distinct(FL_DATE,
           ORIGIN,
           FLIGHTS,
           CANCELLED) %>%
  arrange(FL_DATE) %>%
  # Summarise total flights and cancelled flights nationally
  group_by(FL_DATE) %>%
  mutate(TOTAL_FLIGHTS = sum(FLIGHTS),
         TOTAL_CANCELLED = sum(CANCELLED)) %>%
  ungroup() %>%
  # Find the rolling number of flights delayed by airport and naturally
  group_by(ORIGIN) %>%
  mutate(ROLLING_FLIGHTS = rollapplyr(FLIGHTS, 14, sum, partial = TRUE),
         ROLLING_CANCELLED = rollapplyr(CANCELLED, 14, sum, partial = TRUE),
         ROLLING_TOTAL_FLIGHTS = rollapplyr(TOTAL_FLIGHTS, 14, sum, partial = TRUE),
         ROLLING_TOTAL_CANCELLED = rollapplyr(TOTAL_CANCELLED, 14, sum, partial = TRUE),
         PCT_CANCELLED = ROLLING_CANCELLED/ROLLING_FLIGHTS,
         PCT_TOTAL_CANCELLED = ROLLING_TOTAL_CANCELLED/ROLLING_TOTAL_FLIGHTS) %>%
  ungroup()


  # Identify the most recent two weeks and the prior two weeks
mark2 <- mark1 %>%
  mutate(weeks = ifelse(FL_DATE >= max(FL_DATE)-14, 1, 0),
         weeks = ifelse(FL_DATE < max(FL_DATE)-14 & FL_DATE >= max(FL_DATE)-28, 2, weeks)) %>%
  group_by(ORIGIN,
           weeks) %>%
  mutate(AVG_CANCELLATIONS = mean(CANCELLED/FLIGHTS),
         AVG_CANCELLATIONS = ifelse(weeks == 0, NA, AVG_CANCELLATIONS)) %>%
  ungroup() %>%
  group_by(ORIGIN) %>%
  mutate(WEEK_CHG = AVG_CANCELLATIONS - lag(AVG_CANCELLATIONS)) %>%
  filter(weeks == 1) %>%
  filter(FL_DATE == min(FL_DATE)) %>%
  ungroup() %>%
  select(ORIGIN,
         WEEK_CHG) %>%
  mutate(trajectory = ifelse(WEEK_CHG >= 0.05, 1, 0),
         trajectory = ifelse(WEEK_CHG <= -0.05, -1, trajectory),
         trajectory = ifelse(WEEK_CHG < 0.05 & WEEK_CHG > -0.05, 0, trajectory))

head(mark1)
head(mark2)

mark3 <- inner_join(mark1, mark2, by = c('ORIGIN' = 'ORIGIN'))


ggplot(subset(mark3, trajectory == 0),
       aes(x = FL_DATE,
           y = CANCELLED/FLIGHTS,
           group = ORIGIN)) +
  geom_hline(yintercept = 0) +
  geom_bar(stat = 'identity',
           position = 'identity',
           fill = '#c5eddf') +
  geom_bar(data = subset(mark3, trajectory == 0 & FL_DATE >= max(FL_DATE)-14),
           mapping = aes(x = FL_DATE,
                         y = CANCELLED/FLIGHTS,
                         group = ORIGIN),
           stat = 'identity',
           position = 'identity',
           fill = '#5d8abd') +
  geom_line(data = subset(mark3, trajectory == 0),
            mapping = aes(x = FL_DATE,
                          y = PCT_TOTAL_CANCELLED,
                          group = ORIGIN),
            color = 'black',
            size = 1) +
  geom_line(data = subset(mark3, trajectory == 0),
            mapping = aes(x = FL_DATE,
                          y = PCT_CANCELLED,
                          group = ORIGIN),
            color = '#00429d',
            size = 1) +
  facet_wrap(~ORIGIN) +
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title = element_text(face = 'bold', size = 18, family = 'Arial'),
        legend.position = 'top',
        legend.background=element_blank(),
        plot.subtitle = element_text(size = 15, family = 'Arial'),
        plot.caption = element_text(size = 12, family = 'Arial'),
        axis.title = element_text(size = 12, family = 'Arial'),
        axis.text = element_text(size = 8, family = 'Arial'),
        strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'black', family = 'Arial'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.y = element_line(colour = "gray", linetype = "dashed"),
        panel.grid.major.x = element_blank()) 




ggplot(subset(mark1, ORIGIN %in% c('DSM', 'OMA', 'LNK', 'MCI', 'STL', 'LAS')),
       aes(x = FL_DATE,
           y = PCT_CANCELLED,
           group = ORIGIN,
           color = ORIGIN)) +
  geom_hline(yintercept = 0) +
  # Each airport 
  geom_line(size = 1,
            alpha = 0.7) +
  geom_dl(subset(mark1, ORIGIN %in% c('DSM', 'OMA', 'LNK', 'MCI', 'STL', 'LAS')),
          mapping = aes(x = FL_DATE,
                        y = PCT_CANCELLED,
                        label = ORIGIN), 
          method = list(dl.combine("last.bumpup")), 
          size = 5,
          cex = 0.8) +
  # The nation overall
  geom_line(subset(mark1, ORIGIN == 'MDW'),
            mapping = aes(x = FL_DATE,
                          y = PCT_TOTAL_CANCELLED),
            size = 1,
            color = 'black') +
  geom_dl(subset(mark1, ORIGIN == 'MDW'),
          mapping = aes(x = FL_DATE,
                        y = PCT_TOTAL_CANCELLED,
                        label = 'Nation Overall'), 
          color = 'black',
          size = 5,
          method = list(dl.combine("last.points")), 
          cex = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title = element_text(face = 'bold', size = 18, family = 'Arial'),
        legend.position = 'top',
        legend.background=element_blank(),
        plot.subtitle = element_text(size = 15, family = 'Arial'),
        plot.caption = element_text(size = 12, family = 'Arial'),
        axis.title = element_text(size = 12, family = 'Arial'),
        axis.text = element_text(size = 12, family = 'Arial'),
        strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'brown', family = 'Arial'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.y = element_line(colour = "gray", linetype = "dashed"),
        panel.grid.major.x = element_blank()) 

#usmap::plot_usmap("states", labels = FALSE)
#geom_line()