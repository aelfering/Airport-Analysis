# How has travel changed at Eppley Airfield between 1993 and 2019?
# Using survey results from DB1B and DB1A

# Script by Alex Elfering

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
library(stringr)

options(scipen = 999)

db1ba <- list.files("/Users/alexelfering/Desktop/TicketFare", pattern = "*.csv", full.names = TRUE)
db1badf <- rbindlist(lapply(db1ba, fread))

oma_db <- subset(db1badf, ORIGIN == 'OMA')

# How many passengers are traveling via non-stop, or multiple connections?
airport_group_passengers <- oma_db %>%
  group_by(YEAR,
           AIRPORT_GROUP) %>%
  summarise(PASSENGERS = sum(PASSENGERS)) %>%
  ungroup() %>%
  group_by(YEAR) %>%
  mutate(TOT_PAX = sum(PASSENGERS)) %>%
  ungroup() %>%
  mutate(MKT_SHARE = PASSENGERS/TOT_PAX)

counting_connections <- airport_group_passengers %>%
  mutate(AIRPORT_GROUP = gsub('\\:', ' ', AIRPORT_GROUP)) %>%
  # TOTAL_CONNECTIONS counts how many airports passengers pass through while on their way to the final destination excluding origin and final destination
  mutate(TOTAL_CONNECTIONS = sapply(strsplit(AIRPORT_GROUP, " "), length)-2) %>%
  mutate(FINAL_DEST = word(AIRPORT_GROUP, -1)) %>%
  mutate(SHARE_RANK = dense_rank(desc(MKT_SHARE))) %>%
  arrange(desc(MKT_SHARE))

connection_sum <- counting_connections %>%
  mutate(CONNECTIONS_MADE = ifelse(TOTAL_CONNECTIONS == 0, 'Non-Stop',
                                   ifelse(TOTAL_CONNECTIONS == 1, '1 Connection',
                                          ifelse(TOTAL_CONNECTIONS == 2, '2 Connections',
                                                 ifelse(TOTAL_CONNECTIONS == 3, '3 Connections', '+4 Connections'))))) %>%
  group_by(YEAR,
           CONNECTIONS_MADE) %>%
  summarise(PASSENGERS = sum(PASSENGERS)) %>%
  ungroup() %>%
  group_by(YEAR) %>%
  mutate(YEARLY_PAX = sum(PASSENGERS)) %>%
  ungroup() %>%
  mutate(PercentofCustomers = PASSENGERS/YEARLY_PAX)

# Shows roughly how many connections that passengers made to their final destination
# Interesting note: The percent of passngers taking non-stop flights increases sharply after 2015
ggplot(connection_sum, aes(x = YEAR, y = PercentofCustomers)) +
  geom_hline(yintercept = 0,
             linetype = 'dashed') +
  geom_line(aes(group = CONNECTIONS_MADE,
                color = CONNECTIONS_MADE),
            size = 1) +
  geom_point(aes(group = CONNECTIONS_MADE,
                 color = CONNECTIONS_MADE),
             size = 1) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = 'How many connections passengers at Eppley Airfield make to their final destination',
       y = 'percent of passengers',
       x = 'year') +
  theme(legend.position = 'top')

head(oma_db)

test <- oma_db %>%
  mutate(AIRPORT_GROUP = gsub('\\:', ' ', AIRPORT_GROUP)) %>%
  filter(sapply(strsplit(AIRPORT_GROUP, " "), length)-2 == 0) %>%
  unique(YEAR, AIRPORT_GROUP, TICKET_CARRIER)