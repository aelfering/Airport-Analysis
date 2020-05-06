library(lubridate)
library(dplyr)
library(tidyr)
library(tidyverse)
library(scales)
library(reshape2)
library(data.table)
library(directlabels)
library(RcppRoll)
library(zoo)
library(anytime)

options(scipen = 999)

# Data load and remove NA values -----------------
market_passengers <- list.files("/Users/alexelfering/Desktop/TicketFare", pattern = "*.csv", full.names = TRUE)
marketdf <- rbindlist(lapply(market_passengers, fread))

oma_airport <- subset(marketdf, ORIGIN == 'LNK' & DEST_COUNTRY == 'US')

head(oma_airport)

mark1 <- oma_airport %>%
  group_by(YEAR,
           TICKET_CARRIER) %>%
  summarise(PASSENGERS = sum(PASSENGERS)) %>%
  ungroup() %>%
  group_by(YEAR) %>%
  mutate(TOT_PAX = sum(PASSENGERS)) %>%
  ungroup() %>%
  mutate(MKT_SHARE = PASSENGERS/TOT_PAX)

mark2 <- oma_airport %>%
  group_by(YEAR,
           AIRPORT_GROUP) %>%
  summarise(PASSENGERS = sum(PASSENGERS)) %>%
  ungroup() %>%
  group_by(YEAR) %>%
  mutate(TOT_PAX = sum(PASSENGERS)) %>%
  ungroup() %>%
  mutate(MKT_SHARE = PASSENGERS/TOT_PAX)

pop_routes <- mark2 %>%
  filter(YEAR == max(YEAR)) %>%
  mutate(SHARE_RANK = dense_rank(desc(MKT_SHARE))) %>%
  filter(SHARE_RANK <= 100) %>%
  arrange(desc(MKT_SHARE))

pop_route_names <- as.character(pop_routes$AIRPORT_GROUP)

ggplot(subset(mark2, AIRPORT_GROUP %in% pop_route_names), aes(x = YEAR, y = MKT_SHARE)) +
         geom_line(aes(group = AIRPORT_GROUP)) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~AIRPORT_GROUP)

ggplot(subset(mark1, TICKET_CARRIER %in% c('WN', 'UA', 'AA', 'NW', 'CO', 'G4', 'AS', 'US', 'F9', 'TW', 'DL', 'HP')), aes(x = YEAR, y = MKT_SHARE)) + 
  geom_line(aes(group = TICKET_CARRIER)) + 
  #geom_point(aes(group = TICKET_CARRIER))+
  facet_wrap(~TICKET_CARRIER) +
  geom_hline(yintercept = 0,
             linetype = 'dashed')





