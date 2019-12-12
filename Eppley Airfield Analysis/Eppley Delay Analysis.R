# Exploring delays via the Bureau of Transportation Statistics

# Library load --------------
library(data.table, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(magrittr, warn.conflicts = FALSE)
library(bbplot, warn.conflicts = FALSE)
library(reshape2, warn.conflicts = FALSE)
library(egg, warn.conflicts = FALSE)

options(scipen = 999)

# Data load and remove NA values -----------------
market_delays <- list.files("/Users/alexelfering/Desktop/FlightDelays", pattern = "*.csv", full.names = TRUE)
market <- rbindlist(lapply(market_delays, fread))

market[is.na(market)] <- 0

market <- as.data.frame(market)

# Create a standard dataframe with Eppley Airfield ------------------------------------------------------------------
omadf <- market %>%
  filter(ORIGIN == 'OMA', CANCELLED != 1, DIVERTED != 1)
# What is the YTD delay rate? ------------------------------------------------------------------
delays <- omadf %>%
  select(YEAR, 
         MONTH, 
         DEP_DEL15, 
         FLIGHTS, 
         DEP_DELAY_NEW) %>%
  group_by(YEAR, 
           MONTH) %>%
  summarise(Delays = sum(DEP_DEL15),
            Flights = sum(FLIGHTS),
            Delay.Minutes = sum(DEP_DELAY_NEW))

delays.19 <- delays %>%
  filter(YEAR == 2019)

delays.18 <- delays %>%
  filter(YEAR == 2018)

delays.ytd <- left_join(delays.19, delays.18, by = c('MONTH' = 'MONTH')) #  left joining compares ytd

# final stats
final.stats <- delays.ytd %>%
  select(YEAR = YEAR.x, 
         Delays19 = Delays.x, 
         Flights19 = Flights.x,
         Delay.Minutes19 = Delay.Minutes.x,
         Delays18 = Delays.y,
         Flights18 = Flights.y,
         Delay.Minutes18 = Delay.Minutes.y) %>%
  group_by(YEAR) %>%
  summarise(Delays19 = sum(Delays19),
            Flights19 = sum(Flights19),
            Delay.Minutes19 = sum(Delay.Minutes19),
            Delays18 = sum(Delays18),
            Flights18 = sum(Flights18),
            Delay.Minutes18 = sum(Delay.Minutes18)) %>%
  mutate(Delay.Rate.19 = Delays19/Flights19,
         Delay.Rate.18 = Delays18/Flights18,
         Rate.Change = Delays19/Flights19 - Delays18/Flights18,
         Minute.Chg = (Delay.Minutes19-Delay.Minutes18)/Delay.Minutes18) %>%
  select(Delay.Rate.19,
         Delay.Rate.18,
         Rate.Change,
         Delay.Minutes19,
         Delay.Minutes18,
         Minute.Chg)

# Flight delays broken out by reasons ---------------------------------------------
delays.reasons <- omadf %>%
  select(YEAR, 
         MONTH, 
         WEATHER_DELAY,
         LATE_AIRCRAFT_DELAY,
         SECURITY_DELAY,
         NAS_DELAY,
         CARRIER_DELAY) %>%
  group_by(YEAR, 
           MONTH) %>%
  summarise(All.Delay.Minutes = sum(WEATHER_DELAY) + sum(LATE_AIRCRAFT_DELAY) + sum(SECURITY_DELAY) + sum(NAS_DELAY) + sum(CARRIER_DELAY),
            Weather.Delay = sum(WEATHER_DELAY),
            Late.Aircraft.Delay = sum(LATE_AIRCRAFT_DELAY),
            Security.Delay = sum(SECURITY_DELAY),
            NAS.Delay = sum(NAS_DELAY),
            Carrier.Delay = sum(CARRIER_DELAY))

delays.19.reasons <- delays.reasons %>%
  filter(YEAR == 2019) %>%
  group_by(YEAR) %>%
  summarise(All.Minutes = sum(All.Delay.Minutes),
            Weather = sum(Weather.Delay),
            Late.Aircraft = sum(Late.Aircraft.Delay),
            Security = sum(Security.Delay),
            NAS = sum(NAS.Delay),
            Carrier = sum(Carrier.Delay)) %>%
  ungroup() %>%
  mutate(Weather = Weather/All.Minutes,
         Late.Aircraft = Late.Aircraft/All.Minutes,
         Security = Security/All.Minutes,
         NAS = NAS/All.Minutes,
         Carrier = Carrier/All.Minutes) %>%
  select(Weather,Late.Aircraft, Security, NAS, Carrier)

delays.19 <- melt(delays.19.reasons)

graph.19 <- ggplot(delays.19, aes(x = reorder(variable, value), y = value*100, label = scales::percent(value))) +
  geom_bar(stat="identity", 
           position="identity", 
           fill="#1380A1") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_text(position = position_dodge(width = 1), size = 5, hjust = -0.1) +
  bbc_style() +
  coord_flip() +
  labs(title="Percent of Delays by Reason",
       subtitle = "For Omaha Eppley Airfield YTD 2019")

finalise_plot(plot_name = graph.19,
              source = "Visual by Alex Elfering | Source: Eppley Airfield",
              save_filepath = "Eppley_Passengers.png",
              width_pixels = 1140,
              height_pixels = 550)

# What is the biggest source of delays? ------------------------------------------------------------------
carrier.delays <- omadf %>%
  select(YEAR, 
         MONTH, 
         MKT_UNIQUE_CARRIER,
         DEP_DEL15, 
         FLIGHTS, 
         DEP_DELAY_NEW) %>%
  group_by(YEAR, 
           MONTH, 
           MKT_UNIQUE_CARRIER) %>%
  summarise(Delays = sum(DEP_DEL15),
            Flights = sum(FLIGHTS),
            Delay.Minutes = sum(DEP_DELAY_NEW))
  
carrier.18 <- subset(carrier.delays, carrier.delays$YEAR == 2018)
carrier.19 <- subset(carrier.delays, carrier.delays$YEAR == 2019)

carrier <- left_join(carrier.19, carrier.18, by = c('MONTH' = 'MONTH', 'MKT_UNIQUE_CARRIER' = 'MKT_UNIQUE_CARRIER'))

carrier %>%
  group_by(MKT_UNIQUE_CARRIER) %>%
  summarise(Delay19 = sum(Delay.Minutes.x),
            Delay18 = sum(Delay.Minutes.y)) %>%
  mutate(Pct.Chg = (Delay19-Delay18)/Delay18)







