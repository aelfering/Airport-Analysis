# EPPLEY AIRFIELD ANALYSIS
# Script written by Alex Elfering

#### LOADING THE LIBRARIES ####
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

####  LOADING THE DATA AND FILTERING FOR OMAHA ####
db1ba <- list.files("/Users/alexelfering/Desktop/DB1BA/market", pattern = "*.csv", full.names = TRUE)
db1badf <- rbindlist(lapply(db1ba, fread))

oma_db <- subset(db1badf, ORIGIN == 'OMA')

head(oma_db)

####  Are more customers traveling non-stop, or connecting? ####
connection_movement <- oma_db %>%
  mutate(AIRPORT_GROUP = gsub('\\:', ' ', AIRPORT_GROUP)) %>%
  mutate(TOTAL_CONNECTIONS = sapply(strsplit(AIRPORT_GROUP, " "), length)-2) %>%
  group_by(YEAR,
           TOTAL_CONNECTIONS) %>%
  summarise(PAX = sum(PASSENGERS)) %>%
  ungroup() %>%
  group_by(YEAR) %>%
  mutate(YEARLY_PAX = sum(PAX)) %>%
  ungroup() %>%
  mutate(SHARE = PAX/YEARLY_PAX) %>%
  filter(TOTAL_CONNECTIONS <= 3)

ggplot(connection_movement,
       aes(x = YEAR,
           y = SHARE,
           group = TOTAL_CONNECTIONS,
           color = as.factor(TOTAL_CONNECTIONS))) +
  geom_line(size = 1) +
  scale_y_continuous(labels = scales::percent)

####  connecctions by dest ####
dest_conn <- oma_db %>%
  filter(TICKET_CARRIER != '99') %>%
  mutate(AIRPORT_GROUP = gsub('\\:', ' ', AIRPORT_GROUP)) %>%
  mutate(TOTAL_CONNECTIONS = sapply(strsplit(AIRPORT_GROUP, " "), length)-2) %>%
  group_by(YEAR,
           DEST,
           TOTAL_CONNECTIONS) %>%
  summarise(PAX = sum(PASSENGERS)) %>%
  ungroup() %>%
  group_by(YEAR,
           TOTAL_CONNECTIONS) %>%
  mutate(CONNEC_PAX = sum(PAX)) %>%
  ungroup() %>%
  group_by(YEAR) %>%
  mutate(YEARLY_PAX = sum(PAX)) %>%
  ungroup() %>%
  group_by(YEAR,
           DEST) %>%
  mutate(CARRIER_PAX = sum(PAX)) %>%
  ungroup() %>%
  mutate(CARRIER_SHARE = PAX/CARRIER_PAX)

top_dest_conn <- dest_conn %>%
  filter(YEAR == 2019) %>%
  group_by(YEAR) %>%
  mutate(MKT_SHARE = CARRIER_PAX/YEARLY_PAX) %>%
  mutate(DEST_RANK = dense_rank(desc(MKT_SHARE))) %>%
  filter(DEST_RANK <= 30)

top_dest_conn_names <- as.character(unique(top_dest_conn$DEST))

ggplot(subset(dest_conn, DEST %in% top_dest_conn_names),
       aes( x = YEAR,
            y = CARRIER_SHARE,
            group = TOTAL_CONNECTIONS,
            color = factor(TOTAL_CONNECTIONS))) +
  geom_line() + 
  facet_wrap(~DEST,
             ncol = 8)

####  Connections by Airline ####

connection_carriers <- oma_db %>%
  filter(TICKET_CARRIER != '99') %>%
  mutate(AIRPORT_GROUP = gsub('\\:', ' ', AIRPORT_GROUP)) %>%
  mutate(TOTAL_CONNECTIONS = sapply(strsplit(AIRPORT_GROUP, " "), length)-2) %>%
  group_by(YEAR,
           TICKET_CARRIER,
           TOTAL_CONNECTIONS) %>%
  summarise(PAX = sum(PASSENGERS)) %>%
  ungroup() %>%
  group_by(YEAR,
           TOTAL_CONNECTIONS) %>%
  mutate(CONNEC_PAX = sum(PAX)) %>%
  ungroup() %>%
  group_by(YEAR) %>%
  mutate(YEARLY_PAX = sum(PAX)) %>%
  ungroup() %>%
  group_by(YEAR,
           TICKET_CARRIER) %>%
  mutate(CARRIER_PAX = sum(PAX)) %>%
  ungroup() %>%
  mutate(CARRIER_SHARE = PAX/CARRIER_PAX) %>%
  filter(TOTAL_CONNECTIONS <= 2)

ggplot(subset(connection_carriers, TICKET_CARRIER %in% top_carrier_names),
       aes(x = YEAR,
           y = CARRIER_SHARE,
           group = TOTAL_CONNECTIONS,
           color = factor(TOTAL_CONNECTIONS))) +
  geom_line(size = 1) +
  facet_wrap(~TICKET_CARRIER)

carrier_connection <- oma_db %>%
  filter(TICKET_CARRIER != '99') %>%
  mutate(AIRPORT_GROUP = gsub('\\:', ' ', AIRPORT_GROUP)) %>%
  mutate(TOTAL_CONNECTIONS = sapply(strsplit(AIRPORT_GROUP, " "), length)-2) %>%
  group_by(YEAR,
           TICKET_CARRIER,
           TOTAL_CONNECTIONS) %>%
  summarise(PAX = sum(PASSENGERS)) %>%
  ungroup() %>%
  group_by(YEAR,
           TOTAL_CONNECTIONS) %>%
  mutate(CONNEC_PAX = sum(PAX)) %>%
  ungroup() %>%
  mutate(CONNEC_SHARE = PAX/CONNEC_PAX) %>%
  group_by(YEAR) %>%
  mutate(YEARLY_PAX = sum(PAX)) %>%
  ungroup() %>%
  group_by(YEAR,
           TICKET_CARRIER) %>%
  mutate(CARRIER_PAX = sum(PAX)) %>%
  ungroup() %>%
  filter(TOTAL_CONNECTIONS <= 2)

ggplot(carrier_connection,
       aes(x = YEAR,
           y = CONNEC_SHARE)) +
  geom_line(color = 'gray',
            aes(group = TICKET_CARRIER),
            size = 1,
            alpha = 0.4) +
  geom_line(data = subset(carrier_connection, TICKET_CARRIER %in% top_carrier_names),
            mapping = aes(x = YEAR,
                          y = CONNEC_SHARE,
                          group = TICKET_CARRIER,
                          color = TICKET_CARRIER),
            size = 1) + 
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = 'top') +
  labs(title = 'Southwest Airlines Carries More Passengers Traveling Non-Stop And, More Recently, Conencting Through 1 Airport',
       subtitle = ' ',
       caption = 'Visualization by Alex Elfering\nSource: DB1B & DB1A',
       y = 'Market Share of Passengers',
       x = 'Year') +
  facet_wrap(~TOTAL_CONNECTIONS,
             ncol = 3)


####  Top Destinations ####

dest_pax <- oma_db %>%
  mutate(AIRPORT_GROUP = gsub('\\:', ' ', AIRPORT_GROUP)) %>%
  mutate(FINAL_DEST = word(AIRPORT_GROUP, -1)) %>%
  group_by(YEAR,
           FINAL_DEST) %>%
  summarise(PAX = sum(PASSENGERS)) %>%
  ungroup() %>%
  group_by(YEAR) %>%
  mutate(YEARLY_PAX = sum(PAX)) %>%
  ungroup() %>%
  mutate(MKT_SHARE = PAX/YEARLY_PAX)

dest_pax %>%
  filter(YEAR == max(YEAR)) %>%
  filter(dense_rank(desc(PAX)) <= 15) %>%
  arrange(desc(MKT_SHARE))
  



dest_mkt_share <- oma_db %>%
  mutate(AIRPORT_GROUP = gsub('\\:', ' ', AIRPORT_GROUP)) %>%
  group_by(YEAR,
           AIRPORT_GROUP) %>%
  summarise(PAX = sum(PASSENGERS)) %>%
  ungroup() %>%
  group_by(YEAR) %>%
  mutate(YEARLY_PAX = sum(PAX)) %>%
  ungroup() %>%
  mutate(MKT_SHARE = PAX/YEARLY_PAX)

head(dest_mkt_share)

top_dest <- dest_mkt_share %>%
  filter(YEAR == 2019) %>%
  #mutate(AIRPORT_GROUP = gsub('\\:', ' ', AIRPORT_GROUP)) %>%
  # TOTAL_CONNECTIONS counts how many airports passengers pass through while on their way to the final destination excluding origin and final destination
  mutate(TOTAL_CONNECTIONS = sapply(strsplit(AIRPORT_GROUP, " "), length)-2) %>%
  mutate(FINAL_DEST = word(AIRPORT_GROUP, -1)) %>%
  #filter(TOTAL_CONNECTIONS > 1) %>%
  mutate(Rank = dense_rank(desc(MKT_SHARE))) %>%
  filter(Rank <= 30)

top_dest_names <- as.character(top_dest$AIRPORT_GROUP)

ggplot(subset(dest_mkt_share, AIRPORT_GROUP %in% top_dest_names),
       aes(x = YEAR,
           y = MKT_SHARE)) +
  geom_hline(yintercept = 0,
             linetype = 'dashed') +
  geom_bar(stat = 'identity',
           position = 'identity',
           fill = 'orange') +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~AIRPORT_GROUP)

reporting_carrier_share <- oma_db %>%
  group_by(YEAR, 
           TICKET_CARRIER) %>%
  summarise(PAX = sum(PASSENGERS)) %>%
  ungroup() %>%
  group_by(YEAR) %>%
  mutate(YEARLY_PAX = sum(PAX)) %>%
  ungroup() %>%
  mutate(MKT_SHARE = PAX/YEARLY_PAX)

top_carrier <- reporting_carrier_share %>%
  filter(YEAR == max(YEAR)) %>%
  mutate(CARRIER_RANK = dense_rank(desc(PAX))) %>%
  filter(CARRIER_RANK <= 10) %>%
  filter(MKT_SHARE > 0.01)

top_carrier_names <- as.character(top_carrier$TICKET_CARRIER)

ggplot(subset(reporting_carrier_share, TICKET_CARRIER %in% top_carrier_names),
       aes(x = YEAR,
           y = MKT_SHARE,
           group = TICKET_CARRIER)) +
  geom_hline(yintercept = 0,
             linetype = 'dashed') +
  geom_line(aes(color = TICKET_CARRIER),
            size = 1) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = 'Approx Market Share Among US Airlines at Eppley Airfield',
       subtitle = 'Grouped by Ticket Carrier',
       caption = 'Source: DB1A & DB1B') +
  theme(legend.position = 'top')
  
  
  
  
