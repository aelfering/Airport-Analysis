#### LOADING THE LIBRARIES ####
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if(length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
}

packages <- c('lubridate', 'dplyr', 'tidyr', 'tidyverse', 'scales', 'reshape2', 'data.table', 'directlabels', 
              'RcppRoll', 'zoo', 'anytime', 'stringr')
ipak(packages)
options(scipen = 999)

db1ba_huge <- list.files("/Users/alexelfering/Desktop/DB1B Whole", pattern = "*.csv", full.names = TRUE)
db1badf_huge <- rbindlist(lapply(db1ba_huge, fread))

# What percent of passengers fly between Chicago and Los Angeles Non-Stop?
non_stop_CHI_LAX <- db1badf_huge %>%
  filter(!TICKET_CARRIER %in% c('--', '99'),
         ORIGIN %in% c('ORD', 'MDW'),
         DEST %in% c('LAX')) %>%
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
one_stop_PHX_LAX <- db1badf_huge %>%
  filter(!TICKET_CARRIER %in% c('--', '99')) %>%
  filter(DEST %in% c('LAX')) %>%
  mutate(AIRPORT_GROUP = gsub('\\:', ' ', AIRPORT_GROUP)) %>%
  # Removing the initial origin and final destination regarding connections
  mutate(TOTAL_CONNECTIONS = sapply(strsplit(AIRPORT_GROUP, " "), length)-2) %>%
  filter(grepl('ORD LAX', AIRPORT_GROUP) | grepl('MDW LAX', AIRPORT_GROUP),
         TOTAL_CONNECTIONS > 0) %>%
  group_by(TICKET_CARRIER) %>%
  summarise(TOTAL_PAX = sum(PASSENGERS)) %>%
  ungroup() %>%
  mutate(QUARTERLY_PAX = sum(TOTAL_PAX)) %>%
  ungroup() %>%
  mutate(MKT_SHARE = TOTAL_PAX/QUARTERLY_PAX)

# What is the combined market share for connecting and non-stop?
con_non <- merge(one_stop_PHX_LAX, non_stop_PHX_LAX, all.x = TRUE, all.y = TRUE, by = c('TICKET_CARRIER' = 'TICKET_CARRIER'))

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
           fill = 'blue',
           color = 'black') +
  labs(title = 'Market Share of Travelers Non-Stop or Connecting to LAX from Chicago',
       y = 'Percent of Travelers',
       x = '',
       caption = 'Q1 of 2019\nVisualization by Alex Elfering\nSource: DB1B') +
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
  labs(title = 'Percent of Travelers Non-Stop or Connecting to LAX from Chicago',
       fill = 'How to Read:',
       y = 'Percent of Travelers',
       x = '',
       caption = 'Q1 of 2019\nVisualization by Alex Elfering\nSource: DB1B') +
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

# What origin feeds Chicago to Los Angeles by carrier?

top_origin_carrier <- db1badf_huge %>%
  filter(!TICKET_CARRIER %in% c('--', '99')) %>%
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
  
  