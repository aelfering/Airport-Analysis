# Exploring delays via the Bureau of Transportation Statistics

# Library load --------------
list.of.packages <- c("dplyr", "ggplot2", "reshape2", "lubridate")
lapply(list.of.packages, require, character.only = TRUE)

options(scipen = 999)

# Data load and remove NA values -----------------
market_delays <- list.files("/Users/alexelfering/Desktop/IA Delays", pattern = "*.csv", full.names = TRUE)
market <- rbindlist(lapply(market_delays, fread))

dsm_flights <- market %>%
  select(FL_DATE,
         MKT_UNIQUE_CARRIER,
         BRANDED_CODE_SHARE,
         MKT_CARRIER,
         MKT_CARRIER_FL_NUM,
         OP_UNIQUE_CARRIER,
         OP_CARRIER_FL_NUM,
         FLIGHTS,
         DEP_DEL15,
         CANCELLED,
         DIVERTED,
         ORIGIN,
         DEST,
         CRS_DEP_TIME,
         DEP_TIME,
         DEP_DELAY,
         DEP_DELAY_NEW,
         WHEELS_OFF,
         CARRIER_DELAY,
         WEATHER_DELAY,
         NAS_DELAY,
         SECURITY_DELAY,
         LATE_AIRCRAFT_DELAY) %>%
  filter(CANCELLED != 1,
         DIVERTED != 1,
         ORIGIN == 'DSM')

dsm_times <- dsm_flights %>%
  select(FL_DATE, 
         Unique.Marketing.Carrier = MKT_UNIQUE_CARRIER, 
         Branded.Code.Share = BRANDED_CODE_SHARE, 
         IATA.Marketing.Carrier = MKT_CARRIER, 
         Unique.Operating.Carrier = OP_UNIQUE_CARRIER,
         Operating.Flight.Number = OP_CARRIER_FL_NUM, 
         Origin = ORIGIN,
         Dest = DEST,
         CRS_DEP_TIME, 
         DEP_TIME, 
         Delay.Minutes = DEP_DELAY,
         Delay.Minutes.Adj = DEP_DELAY_NEW,
         Delay = DEP_DEL15, 
         WHEELS_OFF,
         Flights = FLIGHTS,
         Weather.Delay = WEATHER_DELAY,
         NAS.Delay = NAS_DELAY,
         Security.Delay = SECURITY_DELAY,
         Late.Aircraft.Delay = LATE_AIRCRAFT_DELAY,
         Carrier.Delay = CARRIER_DELAY) %>%
  mutate(FL_DATE = ymd(FL_DATE),
         CRS_DEP_TIME = substr(as.POSIXct(sprintf("%04.0f", CRS_DEP_TIME), format='%H%M'), 12, 16),
         DEP_TIME = substr(as.POSIXct(sprintf("%04.0f", DEP_TIME), format='%H%M'), 12, 16),
         WHEELS_OFF = substr(as.POSIXct(sprintf("%04.0f", WHEELS_OFF), format='%H%M'), 12, 16)) %>%
  mutate(Full.Act.Dep.Time = as.POSIXct(paste(FL_DATE, DEP_TIME), format="%Y-%m-%d %H:%M"),
         Full.CRS.Dep.Time = as.POSIXct(paste(FL_DATE, CRS_DEP_TIME), format="%Y-%m-%d %H:%M"),
         Full.Wheels.Off.Time = as.POSIXct(paste(FL_DATE, WHEELS_OFF), format="%Y-%m-%d %H:%M")) %>%
  select(Full.CRS.Dep.Time,
         Full.Act.Dep.Time,
         Full.Wheels.Off.Time,
         Unique.Marketing.Carrier,
         Branded.Code.Share,
         IATA.Marketing.Carrier,
         Unique.Operating.Carrier,
         Operating.Flight.Number,
         Origin,
         Dest,
         Delay.Minutes,
         Delay.Minutes.Adj,
         Delay,
         Flights,
         NAS.Delay,
         Late.Aircraft.Delay,
         Carrier.Delay,
         Weather.Delay,
         Security.Delay)