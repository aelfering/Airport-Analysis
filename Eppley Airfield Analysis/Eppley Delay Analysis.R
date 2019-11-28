# Exploring delays via the Bureau of Transportation Statistics

# Library load --------------
library(data.table, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(magrittr, warn.conflicts = FALSE)
library(bbplot, warn.conflicts = FALSE)
library(reshape2, warn.conflicts = FALSE)

# Data load and remove NA values -----------------
market_delays <- list.files("/Users/alexelfering/Desktop/FlightDelays", pattern = "*.csv", full.names = TRUE)
market <- rbindlist(lapply(market_delays, fread))

market[is.na(market)] <- 0

# Create a standard dataframe with Eppley Airfield ------------------------------------------------------------------
omadf <- market %>%
  filter(ORIGIN == 'OMA', CANCELLED != 1, DIVERTED != 1)
# What is the YTD delay rate? ------------------------------------------------------------------
delays <- omadf %>%
  select(YEAR, 
         MONTH, 
         DEP_DEL15, 
         FLIGHTS, 
         DEP_DELAY) %>%
  group_by(YEAR, 
           MONTH) %>%
  summarise(Delays = sum(DEP_DEL15),
            Flights = sum(FLIGHTS),
            Delay.Minutes = sum(DEP_DELAY))

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
  summarise(Weather.Delay = sum(WEATHER_DELAY),
            Late.Aircraft.Delay = sum(LATE_AIRCRAFT_DELAY),
            Security.Delay = sum(SECURITY_DELAY),
            NAS.Delay = sum(NAS_DELAY),
            Carrier.Delay = sum(CARRIER_DELAY))

delays.19.reasons <- delays.reasons %>%
  filter(YEAR == 2019)

delays.18.reasons <- delays.reasons %>%
  filter(YEAR == 2018)

delays.ytd.reasons <- left_join(delays.19.reasons, 
                        delays.18.reasons,
                        by = c('MONTH' = 'MONTH')) #  left joining compares ytd

# final stats
final.stats.reasons <- delays.ytd.reasons %>%
  select(YEAR = YEAR.x, 
         Weather19 = Weather.Delay.x, 
         Late.Aircraft19 = Late.Aircraft.Delay.x,
         Security19 = Security.Delay.x,
         NAS19 = NAS.Delay.x,
         Carrier19 = Carrier.Delay.x,
         Weather18 = Weather.Delay.y, 
         Late.Aircraft18 = Late.Aircraft.Delay.y,
         Security18 = Security.Delay.y,
         NAS18 = NAS.Delay.y,
         Carrier18 = Carrier.Delay.y) %>%
  group_by(YEAR) %>%
  summarise(Weather19 = sum(Weather19),
            Late.Aircraft19 = sum(Late.Aircraft19),
            Security19 = sum(Security19),
            NAS19 = sum(NAS19),
            Carrier19 = sum(Carrier19),
            Weather18 = sum(Weather18),
            Late.Aircraft18 = sum(Late.Aircraft18),
            Security18 = sum(Security18),
            NAS18 = sum(NAS18),
            Carrier18 = sum(Carrier18)) %>%
  mutate(Weather = (Weather19 - Weather18)/Weather18,
         Late.Aircraft = (Late.Aircraft19 - Late.Aircraft18)/Late.Aircraft18,
         NAS = (NAS19 - NAS18)/NAS18,
         Carrier = (Carrier19 - Carrier18)/Carrier18,
         Security = (Security19 - Security18)/Security18)%>%
  select(Weather,
         Late.Aircraft,
         NAS,
         Carrier,
         Security)

final.stats.graph <- final.stats.reasons 

# What is the biggest source of delays? ------------------------------------------------------------------

origin <- market %>%
  filter(ORIGIN == 'OMA', CANCELLED != 1) %>%
  group_by(YEAR, MONTH) %>%
  summarise(Delays = sum(DEP_DEL15), 
            Flights = sum(FLIGHTS)) %>%
  ungroup() %>%
  as.data.frame ()

delay_count <- market %>%
  filter(ORIGIN == 'OMA', CANCELLED != 1) %>%
  mutate(Weather_Delays = ifelse(WEATHER_DELAY > 0, 1, 0),
         Carrier_Delays = ifelse(CARRIER_DELAY > 0, 1, 0),
         NAS_Delays = ifelse(NAS_DELAY > 0, 1, 0),
         Aircraft_Delays = ifelse(LATE_AIRCRAFT_DELAY > 0, 1, 0)) %>%
  group_by(YEAR, MONTH, DAY_OF_MONTH, DAY_OF_WEEK) %>%
  summarise(Total_Delays = sum(Weather_Delays) + sum(Carrier_Delays) + sum(NAS_Delays) + sum(Aircraft_Delays),
            Weather_Delays = sum(Weather_Delays),
            Carrier_Delays = sum(Carrier_Delays),
            NAS_Delays = sum(NAS_Delays),
            Aircraft_Delays = sum(Aircraft_Delays)) %>%
  ungroup() %>%
  mutate(Percent.Weather = Weather_Delays/Total_Delays,
         Percent.Carrier = Carrier_Delays/Total_Delays,
         Percent.NAS = NAS_Delays/Total_Delays,
         Percent.Aircraft = NAS_Delays/Total_Delays) %>%
  select(YEAR, 
         MONTH,
         DAY_OF_MONTH,
         DAY_OF_WEEK,
         Total_Delays,
         Percent.Weather, 
         Percent.Carrier, 
         Percent.NAS, 
         Percent.Aircraft) %>%
  as.data.frame()

delay_minutes <- market %>%
  filter(ORIGIN == 'OMA', CANCELLED != 1) %>%
  group_by(YEAR, MONTH) %>%
  summarise(Total_Minutes = sum(WEATHER_DELAY) + sum(NAS_DELAY) + sum(CARRIER_DELAY) + sum(LATE_AIRCRAFT_DELAY),
            Weather_Delays = sum(WEATHER_DELAY),
            NAS_Delays = sum(NAS_DELAY),
            Carrier_Delays = sum(CARRIER_DELAY),
            Aircraft_Delays = sum(LATE_AIRCRAFT_DELAY)) %>%
  ungroup() %>%
  mutate(Year.Month = paste(YEAR, MONTH, sep = ' - '),
         Percent.Weather = Weather_Delays/Total_Minutes,
         Percent.NAS = NAS_Delays/Total_Minutes,
         Percent.Carrier = Carrier_Delays/Total_Minutes,
         Percent.Aircraft = Aircraft_Delays/Total_Minutes) %>%
  select(Year.Month,
         Total_Minutes,
         Percent.Weather,
         Percent.NAS,
         Percent.Carrier,
         Percent.Aircraft) %>%
  as.data.frame()







