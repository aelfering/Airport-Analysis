list.of.packages <- c("ggplot2", "dplyr", 'tidyverse', 'tidyr', 'lubridate', 'data.table', 'zoo', 'ggrepel')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)
library(zoo)
library(ggrepel)

setwd("~/Documents/GitHub/Airport-Analysis/Covid 19 Analysis")

####  TSA Numbers ####
tsa_check <- read.csv('TSA Numbers.csv')

head(tsa_check)

tsa_fields_fixed <- tsa_check %>%
  filter(Travlers != '') %>%
  mutate(Date = mdy(Date),
         Travelers = as.integer(gsub('\\,', '', Travlers)),
         Travelers_LY = as.integer(gsub('\\,', '', Travlers_LY)),
         YOY_Travelers = (Travelers-Travelers_LY)/Travelers_LY,
         Rolling_Travelers = rollapplyr(Travelers, 7, sum, partial = TRUE),
         Rolling_Travelers_LY = rollapplyr(Travelers_LY, 7, sum, partial = TRUE),
         Rolling_YOY_Travelers = (Rolling_Travelers-Rolling_Travelers_LY)/Rolling_Travelers_LY,
         Percent_LY = Travelers/Travelers_LY) %>%
  select(Date,
         Travelers,
         YOY_Travelers,
         Travelers_LY,
         Rolling_YOY_Travelers,
         Percent_LY)

ggplot(tsa_fields_fixed,
       aes(x = Date,
           y = YOY_Travelers)) +
  geom_line(size = 1) +
  geom_line(data = tsa_fields_fixed,
            mapping = aes(x = Date,
                          y = Rolling_YOY_Travelers),
            color = 'orange',
            size = 1)

####  Ranking Airports Based on 2019 Market Data  ####
t100_19 <- read.csv('T100 2019.csv')

head(t100_19)

airport_pax <- t100_19 %>%
  group_by(ORIGIN) %>%
  summarise(PAX = sum(PASSENGERS)) %>%
  ungroup() %>%
  arrange(desc(PAX)) %>%
  filter(row_number() <= 100)

city_pair_pax <- t100_19 %>%
  group_by(ORIGIN,
           DEST) %>%
  summarise(PAX_PAIR = sum(PASSENGERS)) %>%
  ungroup() %>%
  arrange(desc(PAX_PAIR)) %>%
  filter(row_number() <= 100)
  

####  On-Time Data ####
airport_files_cy <- list.files("~/Documents/GitHub/Airport-Analysis/Covid 19 Analysis/Airport Data", pattern = "*.csv", full.names = TRUE)
airport_files_py <- list.files("~/Documents/GitHub/Airport-Analysis/Covid 19 Analysis/Prior Year Data", pattern = "*.csv", full.names = TRUE)

airport_df_cy <- as.data.frame(rbindlist(lapply(airport_files_cy, fread)))
airport_df_py <- as.data.frame(rbindlist(lapply(airport_files_py, fread)))

####  Comparing National Data   ####
overall_cy <- airport_df_cy %>%
  replace(is.na(.), 0) %>%
  mutate(FL_DATE = ymd(FL_DATE)) %>%
  #filter(ORIGIN == 'MDW') %>%
  group_by(FL_DATE,
           MONTH,
           DAY_OF_MONTH) %>%
  summarise(FLIGHTS = sum(FLIGHTS),
            CANCELLED = sum(CANCELLED),
            DELAYED = sum(DEP_DEL15)) %>%
  ungroup() %>%
  mutate(ROLLING_FLIGHTS = rollapplyr(FLIGHTS, 7, mean, partial = TRUE),
         ROLLING_CANCELLED = rollapplyr(CANCELLED, 7, mean, partial = TRUE),
         ROLLING_DELAYED = rollapplyr(DELAYED, 14, mean, partial = TRUE)) %>%
  mutate(ROLLING_PCT_CANCELLED = ROLLING_CANCELLED/ROLLING_FLIGHTS,
         ROLLING_PCT_DELAYED = ROLLING_DELAYED/ROLLING_FLIGHTS)

overall_py <- airport_df_py %>%
  replace(is.na(.), 0) %>%
  mutate(FL_DATE = ymd(FL_DATE)) %>%
  #filter(ORIGIN == 'MDW') %>%
  group_by(FL_DATE,
           MONTH,
           DAY_OF_MONTH) %>%
  summarise(FLIGHTS = sum(FLIGHTS),
            CANCELLED = sum(CANCELLED),
            DELAYED = sum(DEP_DEL15)) %>%
  ungroup() %>%
  mutate(ROLLING_FLIGHTS = rollapplyr(FLIGHTS, 7, mean, partial = TRUE),
         ROLLING_CANCELLED = rollapplyr(CANCELLED, 7, mean, partial = TRUE),
         ROLLING_DELAYED = rollapplyr(DELAYED, 7, mean, partial = TRUE)) %>%
  mutate(ROLLING_PCT_CANCELLED = ROLLING_CANCELLED/ROLLING_FLIGHTS,
         ROLLING_PCT_DELAYED = ROLLING_DELAYED/ROLLING_FLIGHTS)

compare_overall <- inner_join(overall_cy,
                              overall_py,
                              by = c('MONTH' = 'MONTH',
                                     'DAY_OF_MONTH' = 'DAY_OF_MONTH'))

compare_overall_clean <- compare_overall %>%
  select(FL_DATE = FL_DATE.x,
         CY_FLIGHTS = FLIGHTS.x,
         CY_CANCELLED = CANCELLED.x,
         CY_ROLLING_FLIGHTS = ROLLING_FLIGHTS.x,
         CY_ROLLING_CANCELLED = ROLLING_CANCELLED.x,
         CY_PCT_CANCELLED = ROLLING_PCT_CANCELLED.x,
         CY_PCT_DELAYED = ROLLING_PCT_DELAYED.x,
         PY_FLIGHTS = FLIGHTS.y,
         PY_CANCELLED = CANCELLED.y,
         PY_ROLLING_FLIGHTS = ROLLING_FLIGHTS.y,
         PY_ROLLING_CANCELLED = ROLLING_CANCELLED.y,
         PY_PCT_CANCELLED = ROLLING_PCT_CANCELLED.y,
         PY_PCT_DELAYED = ROLLING_PCT_DELAYED.y)

ggplot(compare_overall_clean,
       aes(x = FL_DATE,
           y = CY_FLIGHTS)) +
  geom_hline(yintercept = 0) +
  geom_line(size = 1) +
  geom_line(data = compare_overall_clean,
            mapping = aes(x = FL_DATE,
                          y = CY_CANCELLED),
            color = 'gray',
            size = 1) +
  geom_line(data = compare_overall_clean,
            mapping = aes(x = FL_DATE,
                          y = CY_FLIGHTS-CY_CANCELLED),
            color = 'blue',
            size = 1) +
  labs(title = 'Domestic Flights Operated Between March and April 2020',
       subtitle = 'Despite airlines scheduling less flights, many flights were still cancelled in April',
       x = 'Date',
       y = 'Flights',
       caption = 'Visualization by Alex Elfering\nSource: Bureau of Transportation Statistics') +
  theme(plot.title = element_text(face = 'bold', size = 18, family = 'Arial'),
        legend.position = 'top',
        legend.background=element_blank(),
        plot.subtitle = element_text(size = 15, family = 'Arial'),
        plot.caption = element_text(size = 12, family = 'Arial'),
        axis.title = element_text(size = 12, family = 'Arial'),
        axis.text = element_text(size = 12, family = 'Arial'),
        #strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'black', family = 'Arial'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.y = element_line(colour = "gray", linetype = "dashed"),
        panel.grid.major.x = element_blank()) 
  

ggplot(compare_overall_clean,
       aes(x = FL_DATE,
           y = CY_PCT_CANCELLED)) +
  geom_hline(yintercept = 0) +
  geom_line(data = compare_overall_clean,
            aes(x = FL_DATE,
                y = PY_PCT_CANCELLED),
            color = '#c5eddf',
            size = 1) +
  geom_line(size = 1,
            color = '#73a2c6') +
  scale_y_continuous(labels = scales::percent) +
  labs(title = 'April Saw Less Flights Cancelled Over Time',
       subtitle = 'Rolling 7 Day Sum of Flights Cancelled',
       x = '',
       y = '% Cancelled',
       caption = 'Visualization by Alex Elfering\nSource: Bureau of Transportation Statistics') +
  theme(plot.title = element_text(face = 'bold', size = 18, family = 'Arial'),
        legend.position = 'top',
        legend.background=element_blank(),
        plot.subtitle = element_text(size = 15, family = 'Arial'),
        plot.caption = element_text(size = 12, family = 'Arial'),
        axis.title = element_text(size = 12, family = 'Arial'),
        axis.text = element_text(size = 12, family = 'Arial'),
        #strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'black', family = 'Arial'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.y = element_line(colour = "gray", linetype = "dashed"),
        panel.grid.major.x = element_blank()) 

####  What about from a Carrier Perspective?  ####
carrier_cancelled <- airport_df_cy %>%
  mutate(FL_DATE = ymd(FL_DATE)) %>%
  replace(is.na(.), 0) %>%
  group_by(FL_DATE,
           MKT_UNIQUE_CARRIER) %>%
  mutate(FLIGHTS = sum(FLIGHTS),
         CANCELLED = sum(CANCELLED)) %>%
  ungroup() %>%
  distinct(FL_DATE,
           MKT_UNIQUE_CARRIER,
           FLIGHTS,
           CANCELLED) %>%
  arrange(FL_DATE) %>%
  group_by(FL_DATE) %>%
  mutate(TOTAL_FLIGHTS = sum(FLIGHTS),
         TOTAL_CANCELLED = sum(CANCELLED)) %>%
  ungroup() %>%
  group_by(MKT_UNIQUE_CARRIER) %>%
  mutate(ROLLING_FLIGHTS = rollapplyr(FLIGHTS, 7, sum, partial = TRUE),
         ROLLING_CANCELLED = rollapplyr(CANCELLED, 7, sum, partial = TRUE),
         ROLLING_TOTAL_FLIGHTS = rollapplyr(TOTAL_FLIGHTS, 7, sum, partial = TRUE),
         ROLLING_TOTAL_CANCELLED = rollapplyr(TOTAL_CANCELLED, 7, sum, partial = TRUE),
         PCT_CANCELLED = ROLLING_CANCELLED/ROLLING_FLIGHTS,
         PCT_TOTAL_CANCELLED = ROLLING_TOTAL_CANCELLED/ROLLING_TOTAL_FLIGHTS) %>%
  ungroup()

ggplot(carrier_cancelled,
       aes(x = FL_DATE,
           y = PCT_CANCELLED,
           group = MKT_UNIQUE_CARRIER,
           color = MKT_UNIQUE_CARRIER)) +
  geom_line(size = 1) +
  geom_line(data = carrier_cancelled,
            mapping = aes(x = FL_DATE,
                          y = PCT_TOTAL_CANCELLED),
            color = 'black',
            size = 1)

####  On-Time Analysis ####

head(airport_df_cy)

airport_rolling <- airport_df_cy %>%
  mutate(FL_DATE = ymd(FL_DATE)) %>%
  replace(is.na(.), 0) %>%
  group_by(FL_DATE,
           ORIGIN) %>%
  mutate(FLIGHTS = sum(FLIGHTS),
         CANCELLED = sum(CANCELLED)) %>%
  ungroup() %>%
  distinct(FL_DATE,
           ORIGIN,
           FLIGHTS,
           CANCELLED) %>%
  arrange(FL_DATE) %>%
  group_by(FL_DATE) %>%
  mutate(TOTAL_FLIGHTS = sum(FLIGHTS),
         TOTAL_CANCELLED = sum(CANCELLED)) %>%
  ungroup() %>%
  group_by(ORIGIN) %>%
  mutate(ROLLING_FLIGHTS = rollapplyr(FLIGHTS, 7, sum, partial = TRUE),
         ROLLING_CANCELLED = rollapplyr(CANCELLED, 7, sum, partial = TRUE),
         ROLLING_TOTAL_FLIGHTS = rollapplyr(TOTAL_FLIGHTS, 7, sum, partial = TRUE),
         ROLLING_TOTAL_CANCELLED = rollapplyr(TOTAL_CANCELLED, 7, sum, partial = TRUE),
         PCT_CANCELLED = ROLLING_CANCELLED/ROLLING_FLIGHTS,
         PCT_TOTAL_CANCELLED = ROLLING_TOTAL_CANCELLED/ROLLING_TOTAL_FLIGHTS) %>%
  ungroup()

  # Identify the most recent two weeks and the prior two weeks
mark2 <- airport_rolling %>%
  group_by(ORIGIN) %>%
  filter(FL_DATE >= max(FL_DATE)-14) %>%
  mutate(weeks = ifelse(FL_DATE >= max(FL_DATE)-7, 2, 1)) %>%
  ungroup() %>%
  group_by(ORIGIN,
           weeks) %>%
  summarise(AVG_CANCELLATIONS = mean(CANCELLED/FLIGHTS)) %>%
  ungroup() %>%
  group_by(ORIGIN) %>%
  mutate(WEEK_CHG = AVG_CANCELLATIONS - lag(AVG_CANCELLATIONS)) %>%
  filter(weeks == max(weeks)) %>%
  ungroup() %>%
  select(ORIGIN,
         WEEK_CHG) %>%
  mutate(trajectory = ifelse(WEEK_CHG >= 0.05, 1, 0),
         trajectory = ifelse(WEEK_CHG <= -0.05, -1, trajectory),
         trajectory = ifelse(WEEK_CHG < 0.05 & WEEK_CHG > -0.05, 0, trajectory))

head(mark1)
head(mark2)

mark3 <- inner_join(mark1, mark2, by = c('ORIGIN' = 'ORIGIN'))

ggplot(subset(mark3, trajectory == 1),
       aes(x = FL_DATE,
           y = CANCELLED/FLIGHTS,
           group = ORIGIN)) +
  geom_hline(yintercept = 0) +
  geom_bar(stat = 'identity',
           position = 'identity',
           fill = '#ffdf83') +
  geom_bar(data = subset(mark3, trajectory == 1 & FL_DATE >= max(FL_DATE)-14),
           mapping = aes(x = FL_DATE,
                         y = CANCELLED/FLIGHTS,
                         group = ORIGIN),
           stat = 'identity',
           position = 'identity',
           fill = '#f8bd4b') +
  geom_line(data = subset(mark3, trajectory == 1),
            mapping = aes(x = FL_DATE,
                          y = PCT_TOTAL_CANCELLED,
                          group = ORIGIN),
            color = 'black',
            size = 1,
            alpha = 0.5) +
  geom_line(data = subset(mark3, trajectory == 1),
            mapping = aes(x = FL_DATE,
                          y = PCT_CANCELLED,
                          group = ORIGIN),
            color = '#f3ac39',
            size = 1) +
  geom_line(data = subset(mark3, trajectory == 1 & FL_DATE >= max(FL_DATE)-14),
            mapping = aes(x = FL_DATE,
                          y = PCT_CANCELLED,
                          group = ORIGIN),
            color = '#d8651f',
            size = 1) +
  facet_wrap(~ORIGIN) +
  labs(title = 'Airports Seeing an Increase in Cancellations',
       subtitle = 'The colored line highlights the airport, and the black line is the national rate trending.',
       x = '',
       y = '',
       caption = 'Visualization by Alex Elfering\nInspired by the New York Times') +
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title = element_text(face = 'bold', size = 18, family = 'Arial'),
        legend.position = 'top',
        legend.background=element_blank(),
        plot.subtitle = element_text(size = 15, family = 'Arial'),
        plot.caption = element_text(size = 12, family = 'Arial'),
        axis.title = element_text(size = 12, family = 'Arial'),
        axis.text = element_text(size = 8, family = 'Arial'),
        strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'black', family = 'Arial'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.y = element_line(colour = "gray", linetype = "dashed"),
        panel.grid.major.x = element_blank()) 




ggplot(subset(mark1, ORIGIN %in% c('DSM', 'OMA', 'LNK', 'MCI', 'STL', 'LAS')),
       aes(x = FL_DATE,
           y = PCT_CANCELLED,
           group = ORIGIN,
           color = ORIGIN)) +
  geom_hline(yintercept = 0) +
  # Each airport 
  geom_line(size = 1,
            alpha = 0.7) +
  geom_dl(subset(mark1, ORIGIN %in% c('DSM', 'OMA', 'LNK', 'MCI', 'STL', 'LAS')),
          mapping = aes(x = FL_DATE,
                        y = PCT_CANCELLED,
                        label = ORIGIN), 
          method = list(dl.combine("last.bumpup")), 
          size = 5,
          cex = 0.8) +
  # The nation overall
  geom_line(subset(mark1, ORIGIN == 'MDW'),
            mapping = aes(x = FL_DATE,
                          y = PCT_TOTAL_CANCELLED),
            size = 1,
            color = 'black') +
  geom_dl(subset(mark1, ORIGIN == 'MDW'),
          mapping = aes(x = FL_DATE,
                        y = PCT_TOTAL_CANCELLED,
                        label = 'Nation Overall'), 
          color = 'black',
          size = 5,
          method = list(dl.combine("last.points")), 
          cex = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title = element_text(face = 'bold', size = 18, family = 'Arial'),
        legend.position = 'top',
        legend.background=element_blank(),
        plot.subtitle = element_text(size = 15, family = 'Arial'),
        plot.caption = element_text(size = 12, family = 'Arial'),
        axis.title = element_text(size = 12, family = 'Arial'),
        axis.text = element_text(size = 12, family = 'Arial'),
        strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'brown', family = 'Arial'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.y = element_line(colour = "gray", linetype = "dashed"),
        panel.grid.major.x = element_blank()) 

#usmap::plot_usmap("states", labels = FALSE)
#geom_line()
####  What routes saw the highest cancellations at the end of March? End of April?  ####
head(airport_df)

pair_pct <- airport_df %>%
  mutate(FL_DATE = ymd(FL_DATE)) %>%
  filter(FL_DATE >= '2020-03-01') %>%
  replace(is.na(.), 0) %>%
  group_by(FL_DATE,
           ORIGIN,
           DEST) %>%
  summarise(FLIGHTS = sum(FLIGHTS),
            CANCELLATIONS = sum(CANCELLED)) %>%
  mutate(PAIR = paste(ORIGIN, DEST, sep = '-')) %>%
  ungroup() %>%
  group_by(FL_DATE) %>%
  mutate(TOTAL_FLIGHTS = sum(FLIGHTS),
         TOTAL_CANCELLATIONS = sum(CANCELLATIONS)) %>%
  ungroup() %>%
  filter(ORIGIN %in% airport_pax$ORIGIN) %>%
  group_by(PAIR) %>%
  mutate(ROLLING_FLIGHTS = rollapplyr(FLIGHTS, 14, sum, partial = TRUE),
         ROLLING_CANCELLATIONS = rollapplyr(CANCELLATIONS, 14, sum, partial = TRUE),
         ROLLING_PCT = ROLLING_CANCELLATIONS/ROLLING_FLIGHTS,
         ROLLING_TOTAL_FLIGHTS = rollapplyr(TOTAL_FLIGHTS, 14, sum, partial = TRUE),
         ROLLING_TOTAL_CANCELLATIONS = rollapplyr(TOTAL_CANCELLATIONS, 14, sum, partial = TRUE),
         ROLLING_TOTAL_PCT = ROLLING_TOTAL_CANCELLATIONS/ROLLING_TOTAL_FLIGHTS)

end_month <- c(as.Date('2020-04-16'), as.Date('2020-04-30'))

pair_inc_dec <- pair_pct %>%
  mutate(MONTH = month(FL_DATE)) %>%
  filter(FL_DATE %in% end_month) %>%
  select(MONTH,
         FL_DATE,
         PAIR,
         ROLLING_FLIGHTS,
         ROLLING_CANCELLATIONS,
         ROLLING_PCT) %>%
  group_by(PAIR) %>%
  mutate(CHG = ROLLING_PCT-lag(ROLLING_PCT)) %>%
  ungroup() %>%
  filter(FL_DATE == max(FL_DATE)) %>%
  mutate(trajectory = ifelse(CHG >= 0.05, 1, 0),
         trajectory = ifelse(CHG <= -0.05, -1, trajectory))

inc_pair <- pair_inc_dec %>%
  filter(trajectory == 1) %>%
  arrange(desc(CHG)) %>%
  filter(row_number() <= 50)

same_pair <- pair_inc_dec %>%
  filter(trajectory == 0) %>%
  arrange(desc(ROLLING_FLIGHTS)) %>%
  filter(row_number() <= 100)

dec_pair <- pair_inc_dec %>%
  filter(trajectory == -1) %>%
  arrange(CHG) %>%
  filter(row_number() <= 50)

ggplot(subset(pair_pct, PAIR %in% inc_pair$PAIR),
       aes(x = FL_DATE,
           y = ROLLING_PCT,
           group = PAIR)) +
  geom_line(color = 'gray',
            size = 1) +
  geom_line(color = 'black',
            size = 1,
            data = subset(pair_pct, PAIR %in% inc_pair$PAIR & FL_DATE >= max(FL_DATE)-14),
            mapping = aes(x = FL_DATE,
                          y = ROLLING_PCT,
                          group = PAIR)) +
  facet_wrap(~PAIR)

ggplot(subset(pair_pct, PAIR %in% same_pair$PAIR),
       aes(x = FL_DATE,
           y = ROLLING_PCT,
           group = PAIR)) +
  geom_line(color = 'gray') +
  geom_line(color = 'black',
            data = subset(pair_pct, PAIR %in% same_pair$PAIR & FL_DATE >= max(FL_DATE)-14),
            mapping = aes(x = FL_DATE,
                          y = ROLLING_PCT,
                          group = PAIR)) +
  facet_wrap(~PAIR)

ggplot(subset(pair_pct, PAIR %in% dec_pair$PAIR),
       aes(x = FL_DATE,
           y = ROLLING_PCT,
           group = PAIR)) +
  geom_line(color = '#fbc180',
            size = 1) +
  geom_line(color = '#ea6d25',
            size = 1,
            data = subset(pair_pct, PAIR %in% dec_pair$PAIR & FL_DATE >= max(FL_DATE)-14),
            mapping = aes(x = FL_DATE,
                          y = ROLLING_PCT,
                          group = PAIR)) +
  facet_wrap(~PAIR) +
  theme(plot.title = element_text(face = 'bold', size = 18, family = 'Arial'),
        legend.position = 'top',
        legend.background=element_blank(),
        plot.subtitle = element_text(size = 15, family = 'Arial'),
        plot.caption = element_text(size = 12, family = 'Arial'),
        axis.title = element_text(size = 12, family = 'Arial'),
        axis.text = element_text(size = 8, family = 'Arial'),
        strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'black', family = 'Arial'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.y = element_line(colour = "gray", linetype = "dashed"),
        panel.grid.major.x = element_blank()) 

ggplot(subset(pair_pct, ORIGIN == 'OMA'),
       aes(x = FL_DATE,
           y = ROLLING_PCT,
           group = PAIR)) +
  geom_line() +
  facet_wrap(~PAIR)

ggplot(subset(pair_pct, ORIGIN == 'DSM'),
       aes(x = FL_DATE,
           y = ROLLING_PCT,
           group = PAIR)) +
  geom_line() +
  facet_wrap(~PAIR)
