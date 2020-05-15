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

db1ba_huge <- list.files("/Users/alexelfering/Desktop/DB1B Whole", pattern = "*.csv", full.names = TRUE)
db1badf_huge <- rbindlist(lapply(db1ba_huge, fread))

# What percent of passengers fly between Chicago and Los Angeles Non-Stop?
non_stop_PHX_LAX <- db1badf_huge %>%
  filter(!TICKET_CARRIER %in% c('--', '99'),
         ORIGIN %in% c('ORD', 'MDW'),
         DEST %in% c('LAX'),
         OP_CARRIER_CHANGE == 0) %>%
  mutate(AIRPORT_GROUP = gsub('\\:', ' ', AIRPORT_GROUP)) %>%
  mutate(TOTAL_CONNECTIONS = sapply(strsplit(AIRPORT_GROUP, " "), length)-2) %>%
  filter(TOTAL_CONNECTIONS == 0) %>%
  group_by(TICKET_CARRIER) %>%
  summarise(PAX = sum(PASSENGERS)) %>%
  ungroup() %>%
  mutate(QUARTERLY_PAX = sum(PAX)) %>%
  mutate(MKT_SHARE = PAX/QUARTERLY_PAX)

ggplot(non_stop_PHX_LAX,
       aes(x = reorder(TICKET_CARRIER, MKT_SHARE),
           y = MKT_SHARE)) +
  coord_flip() +
  geom_bar(stat = 'identity',
           position = 'identity',
           color = 'black',
           fill = '#5671d4') +
  geom_text(aes(label = percent(MKT_SHARE)), 
            position = position_dodge(width=1), 
            hjust = 1.25) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = 'United and American Airlines Dominated the Chicago-Los Angeles Segment',
       subtitle = 'Q1 of 2019',
       y = 'Quarterly Market Share',
       x = '',
       caption = 'Source: DB1B\nVisualization by Alex Elfering\nSouthwest flies out of Chicago Midway') +
  theme(plot.title = element_text(face = 'bold', size = 18, family = 'Arial'),
        plot.subtitle = element_text(size = 15, family = 'Arial'),
        plot.caption = element_text(size = 12, family = 'Arial'),
        axis.title = element_text(size = 12, family = 'Arial'),
        axis.text = element_text(size = 12, family = 'Arial'),
        strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'brown', family = 'Arial'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = ggplot2::element_line(color = "#dedede", linetype = 'dashed')) 

# What passengers fly to Los Angeles via Chicago?
one_stop_PHX_LAX <- db1badf_huge %>%
  filter(!TICKET_CARRIER %in% c('--', '99'),
         OP_CARRIER_CHANGE == 0) %>%
  filter(DEST %in% c('LAX')) %>%
  mutate(AIRPORT_GROUP = gsub('\\:', ' ', AIRPORT_GROUP)) %>%
  mutate(TOTAL_CONNECTIONS = sapply(strsplit(AIRPORT_GROUP, " "), length)-2) %>%
  filter(grepl('ORD LAX', AIRPORT_GROUP) | grepl('MDW LAX', AIRPORT_GROUP),
         TOTAL_CONNECTIONS > 0) %>%
  group_by(TICKET_CARRIER) %>%
  summarise(TOTAL_PAX = sum(PASSENGERS)) %>%
  ungroup() %>%
  mutate(QUARTERLY_PAX = sum(TOTAL_PAX)) %>%
  ungroup() %>%
  mutate(MKT_SHARE = TOTAL_PAX/QUARTERLY_PAX)

ggplot(one_stop_PHX_LAX,
       aes(x = reorder(TICKET_CARRIER, MKT_SHARE),
           y = MKT_SHARE)) +
  coord_flip() +
  geom_bar(stat = 'identity',
           position = 'identity',
           color = 'black',
           fill = '#5671d4') +
  geom_text(aes(label = percent(MKT_SHARE)), 
            position = position_dodge(width=1), 
            hjust = 1.25) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = 'United and Southwest Airlines Connect More Passengers Through Chicago to LAX',
       subtitle = 'Q1 of 2019',
       y = 'Quarterly Market Share',
       x = '',
       caption = 'Source: DB1B\nVisualization by Alex Elfering\nSouthwest flies out of Chicago Midway. Alaska does not connect passengers in Chicago.') +
  theme(plot.title = element_text(face = 'bold', size = 18, family = 'Arial'),
        plot.subtitle = element_text(size = 15, family = 'Arial'),
        plot.caption = element_text(size = 12, family = 'Arial'),
        axis.title = element_text(size = 12, family = 'Arial'),
        axis.text = element_text(size = 12, family = 'Arial'),
        strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'brown', family = 'Arial'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = ggplot2::element_line(color = "#dedede", linetype = 'dashed')) 

# What is the combined market share for connecting and non-stop?
con_non <- merge(one_stop_PHX_LAX, non_stop_PHX_LAX, all.x = TRUE, all.y = TRUE, by = c('TICKET_CARRIER' = 'TICKET_CARRIER'))

con_non_mkt_share <- con_non %>%
  select(Airline = TICKET_CARRIER,
         Multi_Stop = TOTAL_PAX,
         Non_Stop = PAX) %>%
  replace(is.na(.), 0) %>%
  mutate(Total_Pax = Multi_Stop + Non_Stop) %>%
  mutate(Quaterly_Pax = sum(Total_Pax)) %>%
  mutate(MKT_SHARE = Total_Pax/Quaterly_Pax)

ggplot(con_non_mkt_share,
       aes(x = reorder(Airline, MKT_SHARE),
           y = MKT_SHARE)) +
  coord_flip() +
  geom_bar(stat = 'identity',
           position = 'identity',
           color = 'black',
           fill = '#5671d4') +
  geom_text(aes(label = percent(MKT_SHARE)), 
            position = position_dodge(width=1), 
            hjust = 1.25) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = 'Overall, American and United Dominate Overall Market Share',
       subtitle = 'Q1 of 2019',
       y = 'Quarterly Market Share',
       x = '',
       caption = 'Source: DB1B\nVisualization by Alex Elfering\nSouthwest flies out of Chicago Midway') +
  theme(plot.title = element_text(face = 'bold', size = 18, family = 'Arial'),
        plot.subtitle = element_text(size = 15, family = 'Arial'),
        plot.caption = element_text(size = 12, family = 'Arial'),
        axis.title = element_text(size = 12, family = 'Arial'),
        axis.text = element_text(size = 12, family = 'Arial'),
        strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'brown', family = 'Arial'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = ggplot2::element_line(color = "#dedede", linetype = 'dashed')) 

# What origin feeds Chicago to Los Angeles by carrier?

top_origin_carrier <- db1badf_huge %>%
  filter(!TICKET_CARRIER %in% c('--', '99'),
         OP_CARRIER_CHANGE == 0) %>%
  filter(DEST %in% c('LAX')) %>%
  mutate(AIRPORT_GROUP = gsub('\\:', ' ', AIRPORT_GROUP)) %>%
  mutate(TOTAL_CONNECTIONS = sapply(strsplit(AIRPORT_GROUP, " "), length)-2) %>%
  filter(grepl('ORD LAX', AIRPORT_GROUP) | grepl('MDW LAX', AIRPORT_GROUP),
         TOTAL_CONNECTIONS > 0) %>%
  group_by(TICKET_CARRIER) %>%
  group_by(TICKET_CARRIER, 
           ORIGIN) %>%
  summarise(PAX = sum(PASSENGERS)) %>%
  ungroup() %>%
  group_by(TICKET_CARRIER) %>%
  mutate(CARRIER_PAX = sum(PAX)) %>%
  mutate(FREQ = PAX/CARRIER_PAX) %>%
  mutate(FREQ_RANK = dense_rank(desc(FREQ))) %>%
  ungroup() %>%
  filter(FREQ_RANK == 1 | ORIGIN == 'ALB') %>%
  arrange(TICKET_CARRIER,
          FREQ_RANK)
  
  