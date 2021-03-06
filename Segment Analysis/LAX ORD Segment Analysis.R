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
library(gridExtra)
library(grid)

options(scipen = 999)

db1b <- list.files("/Users/alexelfering/Desktop/DB1B Whole", pattern = "*.csv", full.names = TRUE)
db1bdf <- rbindlist(lapply(db1b, fread))

str(as.data.frame(db1bdf))

# What passengers fly between Chicago and Los Angeles Non-Stop?
non_stop_CHI_SEA <- db1bdf %>%
  filter(!TICKET_CARRIER %in% c('--', '99'),
         ORIGIN %in% c('ORD', 'MDW'),
         DEST %in% c('SEA')) %>%
  mutate(AIRPORT_GROUP = gsub('\\:', ' ', AIRPORT_GROUP)) %>%
  # Removing the initial origin and final destination regarding connections
  mutate(TOTAL_CONNECTIONS = sapply(strsplit(AIRPORT_GROUP, " "), length)-2) %>%
  filter(TOTAL_CONNECTIONS == 0) %>%
  group_by(TICKET_CARRIER) %>%
  summarise(PAX = sum(PASSENGERS)) %>%
  ungroup() %>%
  mutate(QUARTERLY_PAX = sum(PAX)) %>%
  mutate(MKT_SHARE = PAX/QUARTERLY_PAX)

# What passengers fly to Los Angeles via Chicago?
one_stop_CHI_SEA <- db1bdf %>%
  filter(!TICKET_CARRIER %in% c('--', '99')) %>%
  filter(DEST %in% c('SEA')) %>%
  mutate(AIRPORT_GROUP = gsub('\\:', ' ', AIRPORT_GROUP)) %>%
  # Removing the initial origin and final destination regarding connections
  mutate(TOTAL_CONNECTIONS = sapply(strsplit(AIRPORT_GROUP, " "), length)-2) %>%
  filter(grepl('ORD SEA', AIRPORT_GROUP) | grepl('MDW SEA', AIRPORT_GROUP),
         TOTAL_CONNECTIONS > 0) %>%
  group_by(TICKET_CARRIER) %>%
  summarise(TOTAL_PAX = sum(PASSENGERS)) %>%
  ungroup() %>%
  mutate(QUARTERLY_PAX = sum(TOTAL_PAX)) %>%
  ungroup() %>%
  mutate(MKT_SHARE = TOTAL_PAX/QUARTERLY_PAX) %>%
  # This removes Delta tickets that suggested passengers connected on Delta through Chicago
  filter(MKT_SHARE >= 0.01)

# Why does Alaska Airlines connect passengers? Appears to be American flights that AS pax booked
db1badf_huge %>%
  filter(!TICKET_CARRIER %in% c('--', '99'),
         TICKET_CARRIER == 'AS') %>%
  filter(DEST %in% c('SEA')) %>%
  mutate(AIRPORT_GROUP = gsub('\\:', ' ', AIRPORT_GROUP)) %>%
  # Removing the initial origin and final destination regarding connections
  mutate(TOTAL_CONNECTIONS = sapply(strsplit(AIRPORT_GROUP, " "), length)-2) %>%
  filter(grepl('ORD SEA', AIRPORT_GROUP) | grepl('MDW SEA', AIRPORT_GROUP),
         TOTAL_CONNECTIONS > 0) %>%
  select(TICKET_CARRIER,
         OP_CARRIER_GROUP,
         AIRPORT_GROUP,
         PASSENGERS) %>%
  group_by(TICKET_CARRIER,
           OP_CARRIER_GROUP,
           AIRPORT_GROUP) %>%
  summarise(PASSENGERS = sum(PASSENGERS)) %>%
  ungroup() %>%
  arrange(desc(PASSENGERS))

# What is the combined market share for connecting and non-stop?
con_non <- merge(one_stop_CHI_SEA, non_stop_CHI_SEA, all.x = TRUE, all.y = TRUE, by = c('TICKET_CARRIER' = 'TICKET_CARRIER'))

con_non_mkt_share <- con_non %>%
  select(Airline = TICKET_CARRIER,
         Multi_Stop = TOTAL_PAX,
         Non_Stop = PAX) %>%
  replace(is.na(.), 0) %>%
  mutate(Total_Pax = Multi_Stop + Non_Stop) %>%
  mutate(Quaterly_Pax = sum(Total_Pax)) %>%
  mutate(Mkt_Share = Total_Pax/Quaterly_Pax,
         Pct_Connect = Multi_Stop/Total_Pax,
         Pct_Nonstop = Non_Stop/Total_Pax) %>%
  select(Airline,
         `Market Share` = Mkt_Share,
         `Percent of Passengers Who Connect` = Pct_Connect,
         `Percent of Passengers Non-Stop` = Pct_Nonstop)

# Market Share Graph
ggplot(con_non_mkt_share,
       aes(x = reorder(Airline, `Market Share`),
           y = `Market Share`)) +
  coord_flip() +
  geom_bar(stat = 'identity',
           position = 'identity',
           fill = '#f6955f',
           color = 'black') +
  geom_text(aes(label = paste(round((`Market Share`)*100, 2), "%", sep = '')), 
            position = position_dodge(width=1), 
            hjust = 1.25) +
  labs(title = 'Market Share of Travelers Non-Stop or Connecting to LAX from Chicago',
       y = 'Percent of Travelers',
       x = '',
       caption = 'Visualization by Alex Elfering\nSource: DB1B') +
  scale_y_continuous(labels = scales::percent) +
  geom_hline(yintercept = 0) +
  theme(plot.title = element_text(face = 'bold', size = 18, family = 'Arial'),
        legend.position = 'top',
        plot.subtitle = element_text(size = 15, family = 'Arial'),
        plot.caption = element_text(size = 12, family = 'Arial'),
        axis.title = element_text(size = 12, family = 'Arial'),
        axis.text = element_text(size = 12, family = 'Arial'),
        strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'brown', family = 'Arial'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank()) 

# Percent of Connecting vs. Non-Stop Passengers
con_non_pax <- dplyr::select(con_non_mkt_share, 
                             Airline, 
                             Connecting = `Percent of Passengers Who Connect`, 
                             NonStop = `Percent of Passengers Non-Stop`)

non_stop_pax <- con_non_pax %>%
  mutate(Length = 'Non-Stop') %>%
  select(Airline,
         Length,
         Passengers = NonStop)

connecting_pax <- con_non_pax %>%
  mutate(Length = 'Connecting') %>%
  select(Airline,
         Length,
         Passengers = Connecting)

compare_pax <- rbind(non_stop_pax, connecting_pax)

ggplot(subset(compare_pax, Passengers > 0 & Passengers != 1),
       aes(x = Airline,
           y = Passengers,
           fill = Length)) +
  coord_flip() +
  geom_bar(stat = 'identity',
           position = 'fill') +
  labs(title = 'What is the Ratio of Connecting and Non-Stop Passengers?',
       fill = 'How to Read:',
       y = 'Percent of Travelers',
       x = '',
       caption = 'Delta flies all non-stop and is excluded.\nVisualization by Alex Elfering\nSource: DB1B') +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values= c('#5681b9', '#00429d')) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 0.5,
             linetype = 'dashed') +
  theme(plot.title = element_text(face = 'bold', size = 18, family = 'Arial'),
        legend.position = 'top',
        plot.subtitle = element_text(size = 15, family = 'Arial'),
        plot.caption = element_text(size = 12, family = 'Arial'),
        axis.title = element_text(size = 12, family = 'Arial'),
        axis.text = element_text(size = 12, family = 'Arial'),
        strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'brown', family = 'Arial'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank()) 

# What origin feeds Chicago to Los Angeles by carrier?
top_origin_carrier <- db1badf_huge %>%
  filter(!TICKET_CARRIER %in% c('--', '99', 'DL', 'AS')) %>%
  filter(DEST %in% c('SEA')) %>%
  mutate(AIRPORT_GROUP = gsub('\\:', ' ', AIRPORT_GROUP)) %>%
  mutate(TOTAL_CONNECTIONS = sapply(strsplit(AIRPORT_GROUP, " "), length)-2) %>%
  filter(grepl('ORD SEA', AIRPORT_GROUP) | grepl('MDW SEA', AIRPORT_GROUP),
         TOTAL_CONNECTIONS > 0) %>%
  group_by(TICKET_CARRIER) %>%
  group_by(TICKET_CARRIER, 
           ORIGIN) %>%
  summarise(PAX = sum(PASSENGERS)) %>%
  ungroup() %>%
  group_by(TICKET_CARRIER) %>%
  mutate(CARRIER_PAX = sum(PAX)) %>%
  mutate(FREQ = PAX/CARRIER_PAX,
         FREQ_LABEL = paste(round((PAX/CARRIER_PAX)*100, 2), "%", sep = '')) %>%
  mutate(FREQ_RANK = dense_rank(desc(FREQ))) %>%
  ungroup() %>%
  filter(FREQ_RANK <= 5)

aa <- ggplot(subset(top_origin_carrier, TICKET_CARRIER == 'AA'),
       aes(x = reorder(ORIGIN, FREQ),
           y = FREQ)) +
  coord_flip() +
  geom_bar(stat = 'identity',
           fill = '#d50000',
           color = 'black',
           position = 'identity') +
  scale_y_continuous(labels = scales::percent) +
  geom_hline(yintercept = 0) +
  labs(title = 'American Airlines',
       y = 'Percent of Travelers',
       x = '') +
  geom_text(aes(label = FREQ_LABEL), 
            position = position_dodge(width=1), 
            hjust = 1.25) +
  theme(plot.title = element_text(face = 'bold', size = 15, family = 'Arial'),
        legend.position = 'top',
        plot.subtitle = element_text(size = 15, family = 'Arial'),
        plot.caption = element_text(size = 12, family = 'Arial'),
        axis.title = element_text(size = 12, family = 'Arial'),
        axis.text = element_text(size = 12, family = 'Arial'),
        strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'brown', family = 'Arial'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank()) 

ua <- ggplot(subset(top_origin_carrier, TICKET_CARRIER == 'UA'),
       aes(x = reorder(ORIGIN, FREQ),
           y = FREQ)) +
  coord_flip() +
  geom_bar(stat = 'identity',
           fill = '#2e59a8',
           color = 'black',
           position = 'identity') +
  scale_y_continuous(labels = scales::percent) +
  geom_hline(yintercept = 0) +
  labs(title = 'United Airlines',
       y = 'Percent of Travelers',
       x = '') +
  geom_text(aes(label = FREQ_LABEL), 
            position = position_dodge(width=1), 
            hjust = 1.25) +
  theme(plot.title = element_text(face = 'bold', size = 15, family = 'Arial'),
        legend.position = 'top',
        plot.subtitle = element_text(size = 15, family = 'Arial'),
        plot.caption = element_text(size = 12, family = 'Arial'),
        axis.title = element_text(size = 12, family = 'Arial'),
        axis.text = element_text(size = 12, family = 'Arial'),
        strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'brown', family = 'Arial'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank()) 

wn <- ggplot(subset(top_origin_carrier, TICKET_CARRIER == 'WN'),
       aes(x = reorder(ORIGIN, FREQ),
           y = FREQ)) +
  coord_flip() +
  geom_bar(stat = 'identity',
           fill = '#ff5d2b',
           color = 'black',
           position = 'identity')  +
  scale_y_continuous(labels = scales::percent) +
  geom_hline(yintercept = 0) +
  labs(title = 'Southwest Airlines',
       y = 'Percent of Travelers',
       x = '') +
  geom_text(aes(label = FREQ_LABEL), 
            position = position_dodge(width=1), 
            hjust = 1.25) +
  theme(plot.title = element_text(face = 'bold', size = 15, family = 'Arial'),
        legend.position = 'top',
        plot.subtitle = element_text(size = 15, family = 'Arial'),
        plot.caption = element_text(size = 12, family = 'Arial'),
        axis.title = element_text(size = 12, family = 'Arial'),
        axis.text = element_text(size = 12, family = 'Arial'),
        strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'brown', family = 'Arial'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank()) 
  

grid.arrange(aa, 
             ua, 
             wn, 
             ncol=3,
             top = textGrob("Top Origin Airports Connecting Through Chicago en route to Seattle", 
                            gp = gpar(fontsize = 18, font = 2)),
             bottom = textGrob("Visualization by Alex Elfering\nSource: DB1B", 
                               gp = gpar(fontsize = 10, font = 1)))
