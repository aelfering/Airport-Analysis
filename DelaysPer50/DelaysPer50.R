# This code is used to visualize total delays for every 50 departures among major airports

# Library load --------------
library(data.table, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(magrittr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)

# Data load -----------------
market_delays <- list.files("/Users/alexelfering/Desktop/OnTimeMarket", pattern = "*.csv", full.names = TRUE)
market <- rbindlist(lapply(market_delays, fread))

# Data manipulatins ---------
market <- subset(market, market$YEAR == 2018) # We just want the year of 2018

market <- market %>%
  filter(CANCELLED != 1) %>%
  filter(DIVERTED != 1)
market[is.na(market)] <- 0 # Remove NA, Cancelled, and Diverted flights

# Top Overall Airports--------------
market_overall <- market %>%
  group_by(ORIGIN) %>%
  summarise(FLIGHTS = sum(FLIGHTS)) %>%
  filter(FLIGHTS >= 10000)

# Top Destination Airports-----------------
market_destinations <- market %>%
  group_by(ORIGIN, DEST) %>%
  summarise(FLIGHTS = sum(FLIGHTS)) %>%
  arrange(ORIGIN, desc(FLIGHTS)) %>%
  mutate(rank = rank(desc(FLIGHTS))) %>%
  filter(rank <= 25)

head(market_destinations)

# Delays per 50 Departures Calculation -------------
dest <- market %>%
  select(MONTH, YEAR, ORIGIN, DEST, DISTANCE, DEP_DEL15, FLIGHTS) %>%
  group_by(MONTH, YEAR, ORIGIN, DEST) %>%
  summarise(DISTANCE = mean(DISTANCE), FLIGHTS = sum(FLIGHTS), DELAYS = sum(DEP_DEL15)) %>%
  filter(FLIGHTS >= 50) %>%
  arrange(MONTH, YEAR, desc(FLIGHTS)) %>%
  mutate(rank = dense_rank(desc(FLIGHTS))) %>% 
  mutate(RatePer50 = round(DELAYS * 50)/FLIGHTS)

dest <- merge(dest, market_overall, by = 'ORIGIN')
dest <- merge(dest, market_destinations, by = c('ORIGIN', 'DEST'))

write.csv(dest, file = 'testing.csv')