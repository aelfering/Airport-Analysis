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

oma_db <- subset(db1badf, ORIGIN == 'OMA' & YEAR >= 1999)

head(oma_db)

####  What is the market share among US Airlines? ####
reporting_carrier_share <- oma_db %>%
  filter(!TICKET_CARRIER %in% c('--', '99')) %>%
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

ggplot(reporting_carrier_share,
       aes(x = YEAR,
           y = MKT_SHARE,
           group = TICKET_CARRIER)) +
  geom_line(color = 'gray',
            size = 1,
            alpha = 0.6) +
  geom_line(data = subset(reporting_carrier_share, TICKET_CARRIER %in% top_carrier_names),
            mapping = aes(x = YEAR,
                          y = MKT_SHARE,
                          group = TICKET_CARRIER,
                          color = TICKET_CARRIER),
            size = 1) +
  geom_hline(yintercept = 0,
             linetype = 'dashed') +
  scale_y_continuous(labels = scales::percent) +
  labs(title = 'Market Share Among Airlines at Eppley Airfield',
       caption = 'Visualization by Alex Elfering\nSource: DB1A & DB1B',
       y = 'Market Share',
       x = 'Year',
       color = 'Airline') +
  theme(plot.title = element_text(face = 'bold', size = 18, family = 'Arial'),
        legend.position = 'top',
        plot.subtitle = element_text(size = 15, family = 'Arial'),
        plot.caption = element_text(size = 12, family = 'Arial'),
        axis.title = element_text(size = 12, family = 'Arial'),
        axis.text = element_text(size = 12, family = 'Arial'),
        strip.text = ggplot2::element_text(size = 22, hjust = 0),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.y = element_line(colour = "gray", linetype = "dashed"),
        panel.grid.major.x = element_blank()) +
  guides(colour = guide_legend(nrow = 1))

####   What is the most popular final destination? ####
cy_final_dest <- oma_db %>%
  filter(YEAR == max(YEAR)) %>%
  group_by(YEAR,
           DEST) %>%
  summarise(PAX = sum(PASSENGERS)) %>%
  ungroup() %>%
  group_by(YEAR) %>%
  mutate(YEARLY_PAX = sum(PAX)) %>%
  ungroup() %>%
  mutate(MKT_SHARE = PAX/YEARLY_PAX) %>%
  select(YEAR,
         DEST,
         MKT_SHARE) %>%
  arrange(desc(MKT_SHARE)) %>%
  filter(row_number() <= 10)

py_top_dest <- oma_db %>%
  filter(YEAR == max(YEAR)-10) %>%
  group_by(YEAR,
           DEST) %>%
  summarise(PAX = sum(PASSENGERS)) %>%
  ungroup() %>%
  group_by(YEAR) %>%
  mutate(YEARLY_PAX = sum(PAX)) %>%
  ungroup() %>%
  mutate(MKT_SHARE = PAX/YEARLY_PAX) %>%
  select(YEAR,
         DEST,
         MKT_SHARE) %>%
  arrange(desc(MKT_SHARE)) 

ggplot(cy_final_dest,
       aes(x = reorder(DEST, MKT_SHARE),
           y = MKT_SHARE)) +
  coord_flip() +
  geom_bar(stat = 'identity',
           position = 'identity') +
  scale_y_continuous(labels = scales::percent) +
  geom_hline(yintercept = 0) +
  labs(title = 'Denver, Phoenix, and Las Vegas remain Popular Destinations',
       subtitle = 'Market Share of Passengers in 2019',
       y = 'Market Share',
       x = '',
       caption = 'Visualization by Alex Elfering\nSource: DB1B\nIncludes Passengers Traveling Non-Stop and Connecting')

# This view includes passengers who are travling either non-stop OR connecting
# What about the most popular non-stop destinations?
# This may be skewed by airlines who allow passengers to not change planes, and they continnue onward to their final destination
non_stop <- oma_db %>%
  filter(TICKET_CARRIER != '99') %>%
  mutate(AIRPORT_GROUP = gsub('\\:', ' ', AIRPORT_GROUP)) %>%
  mutate(TOTAL_CONNECTIONS = sapply(strsplit(AIRPORT_GROUP, " "), length)-2) %>%
  group_by(YEAR) %>%
  mutate(YEARLY_PAX = sum(PASSENGERS)) %>%
  ungroup() %>%
  filter(YEAR == max(YEAR),
         TOTAL_CONNECTIONS == 0) %>%
  group_by(YEAR,
           DEST) %>%
  summarise(PAX = sum(PASSENGERS)) %>%
  ungroup() %>%
  group_by(YEAR) %>%
  mutate(YEARLY_PAX = sum(PAX)) %>%
  ungroup() %>%
  mutate(MKT_SHARE = PAX/YEARLY_PAX) %>%
  arrange(desc(MKT_SHARE)) %>%
  filter(row_number() <= 10)

ggplot(non_stop,
       aes(x = reorder(DEST, MKT_SHARE),
           y = MKT_SHARE)) +
  coord_flip() +
  geom_bar(stat = 'identity',
           position = 'identity') +
  scale_y_continuous(labels = scales::percent) +
  geom_hline(yintercept = 0) +
  labs(title = 'Denver, Phoenix, and Las Vegas remain Popular Destinations',
       subtitle = 'Market Share of Passengers in 2019',
       y = 'Market Share',
       x = '',
       caption = 'Visualization by Alex Elfering\nSource: DB1B\nIncludes Passengers Traveling Non-Stop and Connecting')

# What about popular destinations where passengers had to connect?
multi_stop <- oma_db %>%
  filter(TICKET_CARRIER != '99') %>%
  mutate(AIRPORT_GROUP = gsub('\\:', ' ', AIRPORT_GROUP)) %>%
  mutate(TOTAL_CONNECTIONS = sapply(strsplit(AIRPORT_GROUP, " "), length)-2) %>%
  group_by(YEAR) %>%
  mutate(YEARLY_PAX = sum(PASSENGERS)) %>%
  ungroup() %>%
  filter(YEAR == max(YEAR),
         TOTAL_CONNECTIONS != 0) %>%
  group_by(YEAR,
           DEST) %>%
  summarise(PAX = sum(PASSENGERS)) %>%
  ungroup() %>%
  group_by(YEAR) %>%
  mutate(YEARLY_PAX = sum(PAX)) %>%
  ungroup() %>%
  mutate(MKT_SHARE = PAX/YEARLY_PAX) %>%
  arrange(desc(MKT_SHARE)) %>%
  filter(row_number() <= 10)
  
ggplot(multi_stop,
       aes(x = reorder(DEST, MKT_SHARE),
           y = MKT_SHARE)) +
  coord_flip() +
  geom_bar(stat = 'identity',
           position = 'identity') +
  scale_y_continuous(labels = scales::percent) +
  geom_hline(yintercept = 0) +
  labs(title = 'Denver, Phoenix, and Las Vegas remain Popular Destinations',
       subtitle = 'Market Share of Passengers in 2019',
       y = 'Market Share',
       x = '',
       caption = 'Visualization by Alex Elfering\nSource: DB1B\nIncludes Passengers Traveling Non-Stop and Connecting')

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
  
  
  
  


# Which airports are more popular as a final destination, or as a connecting point?
connection_1 <- oma_db %>%
  mutate(AIRPORT_GROUP = gsub('\\:', ' ', AIRPORT_GROUP)) %>%
  mutate(TOTAL_CONNECTIONS = sapply(strsplit(AIRPORT_GROUP, " "), length)-2) %>%
  filter(TOTAL_CONNECTIONS == 1) %>%
  mutate(CONNECTING_AIRPORT = word(AIRPORT_GROUP, 2)) %>%
  group_by(YEAR,
           CONNECTING_AIRPORT) %>%
  summarise(PASSENGERS = sum(PASSENGERS))

final_conn_airport <- inner_join(dest_pax, connection_1, by = c('YEAR' = 'YEAR', 'FINAL_DEST' = 'CONNECTING_AIRPORT'))

pct_conn_final <- final_conn_airport %>%
  select(YEAR,
         AIRPORT = FINAL_DEST,
         CONNECTING_PAX = PASSENGERS,
         FINAL_DEST_PAX = PAX) %>%
  mutate(TOTAL_PAX = CONNECTING_PAX + FINAL_DEST_PAX) %>%
  group_by(YEAR) %>%
  mutate(YEARLY_TOTAL = sum(TOTAL_PAX)) %>%
  ungroup() %>%
  filter(TOTAL_PAX/YEARLY_TOTAL >= 0.01) %>%
  select(YEAR,
         AIRPORT,
         CONNECTING_PAX,
         FINAL_DEST_PAX,
         TOTAL_PAX) %>%
  mutate(PCT_CONN = CONNECTING_PAX/TOTAL_PAX,
         PCT_FINAL = FINAL_DEST_PAX/TOTAL_PAX) %>%
  filter(PCT_CONN >= 0.01) 

ggplot(pct_conn_final,
       aes(x = YEAR,
           y = PCT_CONN,
           group = AIRPORT)) +
  geom_line(#size = 1,
            color = 'blue') +
  geom_line(data = pct_conn_final,
            mapping = aes(x = YEAR,
                          y = PCT_FINAL,
                          group = AIRPORT),
            color = 'orange') +
  facet_wrap(~AIRPORT)