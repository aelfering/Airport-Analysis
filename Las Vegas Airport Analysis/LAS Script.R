# Exploring delays via the Bureau of Transportation Statistics

# Library load --------------
list.of.packages <- c("dplyr", "ggplot2", "reshape2", "lubridate")
lapply(list.of.packages, require, character.only = TRUE)

options(scipen = 999)

# Data load and remove NA values -----------------
market_delays <- list.files("/Users/alexelfering/Desktop/NV Delays", pattern = "*.csv", full.names = TRUE)
market <- rbindlist(lapply(market_delays, fread))

market_filter <- market %>%
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
         CARRIER_DELAY,
         WEATHER_DELAY,
         NAS_DELAY,
         SECURITY_DELAY,
         LATE_AIRCRAFT_DELAY) %>%
  filter(CANCELLED != 1,
         DIVERTED != 1)

time_binded <- market_filter %>%
  select(FL_DATE, 
         Unique.Marketing.Carrier = MKT_UNIQUE_CARRIER, 
         Branded.Code.Share = BRANDED_CODE_SHARE, 
         IATA.Marketing.Carrier = MKT_CARRIER, 
         Unique.Operating.Carrier = OP_UNIQUE_CARRIER,
         Operating.Flight.Number = OP_CARRIER_FL_NUM, 
         CRS_DEP_TIME, 
         DEP_TIME, 
         Delay.Minutes = DEP_DELAY,
         Delay.Minutes.Adj = DEP_DELAY_NEW,
         Delay = DEP_DEL15, 
         Flights = FLIGHTS) %>%
  mutate(FL_DATE = ymd(FL_DATE),
         CRS_DEP_TIME = substr(as.POSIXct(sprintf("%04.0f", CRS_DEP_TIME), format='%H%M'), 12, 16),
         DEP_TIME = substr(as.POSIXct(sprintf("%04.0f", DEP_TIME), format='%H%M'), 12, 16)) %>%
  mutate(Full.Act.Dep.Time = as.POSIXct(paste(FL_DATE, DEP_TIME), format="%Y-%m-%d %H:%M"),
         Full.CRS.Dep.Time = as.POSIXct(paste(FL_DATE, CRS_DEP_TIME), format="%Y-%m-%d %H:%M")) %>%
  group_by(timestamp) %>%
  summarise(Flights = sum(FLIGHTS),
            Delays = sum(DEP_DEL15))

str(time_binded)
