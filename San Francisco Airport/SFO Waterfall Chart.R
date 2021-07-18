# San Francisco International Airport waterfall chart
# examines the change in passenger traffic among international geo regions

# created:  


# load the libraries
library(tidyverse)
library(ggplot2)
library(lubridate)
library(glue)

# load the data
SFOPax <- read.csv('SFO Passengers.csv')

# data cleaning
head(SFOPax)

SFOPaxClean <- SFOPax %>%
  mutate(Year =(substr(Activity.Period, 1, 4)),
         Month = (substr(Activity.Period, 5, 6)),
         Date = ymd(paste(Year, Month, 1, sep = '-') )) %>%
  select(-Activity.Period,
         -Month)
  

# find year-over-year passenger change by geo region
SFOYOYRegion <- SFOPaxClean %>%
  group_by(Year,
           GEO.Region) %>%
  summarise(Pax = sum(Passenger.Count)) %>%
  ungroup() %>%
  filter(Year > 2005) %>%
  group_by(GEO.Region) %>%
  mutate(PY = lag(Pax),
         Diff = Pax-PY,
         YOY = Diff/PY) %>%
  ungroup() %>%
  filter(Year > 2006,
         GEO.Region != 'US')

# find overall year-over-year change
SFOYearPax <- SFOYOYRegion %>%
  group_by(Year) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  ungroup() %>%
  mutate(YOY = Diff/PY)

# variables for testing
YearFilter <- 2012
YearFilterPY <- YearFilter-1

# return pax traffic numbers from YearFilter
SFOYearPaxFilter <- dplyr::filter(SFOYearPax, Year == YearFilter)

PYPaxDF <- tibble(GEO.Region = as.character(YearFilterPY),
                  Pax = as.numeric(SFOYearPaxFilter[,3]))

CYPaxDF <- tibble(GEO.Region = as.character(YearFilter),
                  Pax = as.numeric(SFOYearPaxFilter[,2]))

YearRegionSelect <- SFOYOYRegion %>%
  filter(Year == YearFilter) %>%
  select(GEO.Region,
         Pax = Diff)

# create the waterfall DF
WaterfallDF <- bind_rows(PYPaxDF,
                   YearRegionSelect) %>%
  mutate(RollingPax = cumsum(Pax)) %>%
  bind_rows(CYPaxDF) %>%
  mutate(Rows = row_number()) %>%
  mutate(PY = case_when(Rows == min(Rows) | Rows == max(Rows) ~ 0,
                        TRUE ~ lag(RollingPax)),
         RollingPax = ifelse(is.na(RollingPax), Pax, RollingPax)) %>%
  select(GEO.Region,
         Rows,
         Balance = Pax,
         End = RollingPax,
         Beg = PY)

# set upper and lower limits of the graph
LowerLimit <- WaterfallDF %>%
  summarise(End = min(End)) %>%
  as.numeric()
  
UpperLimit <- WaterfallDF %>%
  summarise(End = max(End)) %>%
  as.numeric()

# the visualization
WaterfallDF %>%
  ggplot() + 
  geom_segment(mapping = aes(x = reorder(GEO.Region, Rows),
                             xend = GEO.Region,
                             y = Beg,
                             yend = End,
                             color = Balance > 0),
               size = 10) +
  geom_text(subset(WaterfallDF, !GEO.Region %in% c(as.character(YearFilter), as.character(YearFilterPY))),
            mapping = aes(x =  GEO.Region,
                          y = ifelse(End > Beg, End, Beg),
                          label = paste0(ifelse(Balance > 0, '+', ''), scales::comma(Balance) )),
            vjust= -0.5,
            family = 'Arial') +
  scale_color_manual(values = c('TRUE' = 'steelblue',
                                'FALSE' = 'darkorange'),
                     labels = c('TRUE' = 'Increased',
                                'FALSE' = 'Decreased')) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  coord_cartesian(ylim = c(LowerLimit * 0.99,
                           UpperLimit * 1.001 )) +
  labs(x = '',
       y = '',
       caption = 'Visualization by Alex Elfering\nSource: San Francisco International Airport',
       color = 'Passenger Traffic...',
       title = paste('San Francisco International Airport YOY Passenger Traffic by International Region'),
       subtitle = paste('Change in passenger traffic ', YearFilter, ' vs ', YearFilterPY, sep = '')) +
  theme(plot.title = element_text(face = 'bold', size = 14, family = 'Arial'),
        plot.subtitle = element_text(face = 'bold', size = 12, family = 'Arial'),
        legend.position = 'top',
        legend.background=element_blank(),
        legend.text = element_text(size = 12, family = 'Arial'),
        legend.title = element_text(size = 12, family = 'Arial'),
        legend.key=element_blank(),
        plot.title.position = "plot",
        plot.caption.position =  "plot",
        plot.caption = element_text(size = 12, family = 'Arial'),
        axis.title = element_text(size = 12, family = 'Arial'),
        axis.text = element_text(size = 12, family = 'Arial', color = '#969696'),
        axis.text.x.bottom = element_text(size = 12, family = 'Arial', color = 'black'),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'black', family = 'Arial'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.y = element_line(colour = "#c1c1c1", linetype = "dashed"),
        panel.grid.major.x = element_blank()) 




