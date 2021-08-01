# San Francisco International Airport waterfall chart
# examines the change in passenger traffic among international geo regions

# created:  20 July 2021


# load the libraries  ----
library(tidyverse)  # for data cleaning and creating new columns 
library(tidylog)    # this will summarise all of the changes that we make to data frames
library(ggplot2)    # for building the viz
library(extrafont)  # customizing the ultimate visualization

# load the data  ----
SFOPax <- read.csv('SFO Passengers.csv')

# data cleaning  ----
head(SFOPax)

SFOPaxClean <- SFOPax %>%
  mutate(Year = as.numeric((substr(Activity.Period, 1, 4))),
         Month = (substr(Activity.Period, 5, 6))) %>%
  select(-Activity.Period,
         -Month) %>%
  filter(GEO.Summary == 'International')

# find year-over-year passenger change by geo region  ----
SFOYOYRegion <- SFOPaxClean %>%
  group_by(Year,
           GEO.Region) %>%
  summarise(Pax = sum(Passenger.Count)) %>%
  ungroup() %>%
  filter(Year > 2005,
         Year < 2021) %>%
  # find the year-over-year change
  group_by(GEO.Region) %>%
  mutate(PY = lag(Pax),
         Diff = Pax-PY) %>%
  ungroup() %>%
  filter(!is.na(PY))

# find overall year-over-year change  ----
SFOYearPax <- SFOYOYRegion %>%
  group_by(Year) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  ungroup()

# variables for testing  ----
YearFilter <- 2019
YearFilterPY <- YearFilter-1

# return pax traffic numbers from YearFilter  ----
SFOYearPaxFilter <- dplyr::filter(SFOYearPax, Year == YearFilter)

PYPaxDF <- tibble(GEO.Region = as.character(YearFilterPY),
                  Amount = as.numeric(SFOYearPaxFilter[,3]))

CYPaxDF <- tibble(GEO.Region = as.character(YearFilter),
                  Amount = as.numeric(SFOYearPaxFilter[,2]))

NetResult <- tibble(GEO.Region = 'Net Gain/Loss',
                    End = as.numeric(SFOYearPaxFilter[,2]),
                    Beg = as.numeric(SFOYearPaxFilter[,3]),
                    Balance = End-Beg)


YearRegionSelect <- SFOYOYRegion %>%
  filter(Year == YearFilter) %>%
  select(GEO.Region,
         Amount = Diff) %>%
  arrange(desc(Amount))

# create the waterfall DF  ----
WaterfallDF <- bind_rows(PYPaxDF,
                         YearRegionSelect) %>%
  mutate(RollingAmount = cumsum(Amount)) %>%
  bind_rows(CYPaxDF) %>%
  #mutate(Rows = row_number()) %>%
  mutate(PY = case_when(GEO.Region %in% c(YearFilter, YearFilterPY) ~ 0,
                        TRUE ~ lag(RollingAmount)),
         RollingPax = ifelse(is.na(RollingAmount), Amount, RollingAmount)) %>%
  select(GEO.Region,
         #Rows,
         Balance = Amount,
         End = RollingPax,
         Beg = PY) %>%
  bind_rows(NetResult) %>%
  mutate(Rows = row_number())

# set upper and lower limits of the graph
LowerLimit <- WaterfallDF %>%
  summarise(End = min(End, na.rm = TRUE)) %>%
  as.numeric()

UpperLimit <- WaterfallDF %>%
  summarise(End = max(End, na.rm = TRUE)) %>%
  as.numeric()

# the visualization
SFOWaterfallChart <- WaterfallDF %>%
  ggplot() + 
  geom_segment(mapping = aes(x = reorder(GEO.Region, Rows),
                             xend = GEO.Region,
                             y = Beg,
                             yend = End,
                             color = Balance > 0),
               size = 10) +
  geom_segment(subset(WaterfallDF, GEO.Region %in% c(as.character(YearFilter), as.character(YearFilterPY))),
               mapping = aes(x = reorder(GEO.Region, Rows),
                             xend = GEO.Region,
                             y = Beg,
                             yend = End),
               color = '#cccccc',
               size = 10) +
  geom_segment(subset(WaterfallDF, !GEO.Region %in% c(as.character(YearFilter), as.character(YearFilterPY))), 
               mapping = aes(x = c(seq(1.7, nrow(WaterfallDF)-2.3, 1), nrow(WaterfallDF)-0.3), 
                             y = Beg, 
                             xend = c(seq(1.7, nrow(WaterfallDF)-2.3, 1), nrow(WaterfallDF)-0.3), 
                             yend = End),
               arrow = arrow(type = "open",
                             length = unit(0.015, "npc")),
               size = 0.5) +
  geom_text(subset(WaterfallDF, !GEO.Region %in% c(as.character(YearFilter), as.character(YearFilterPY))),
            mapping = aes(x =  GEO.Region,
                          y = ifelse(End > Beg, End, Beg),
                          label = paste0(ifelse(Balance > 0, '+', ''), scales::comma(Balance) )),
            vjust= -0.5,
            family = 'Franklin Gothic Book') +
  scale_color_manual(values = c('TRUE' = 'steelblue',
                                'FALSE' = 'darkorange'),
                     labels = c('TRUE' = 'Increased',
                                'FALSE' = 'Decreased')) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  coord_cartesian(ylim = c(LowerLimit * 0.99,
                           UpperLimit * 1.001 )) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  labs(x = '',
       y = '',
       color = 'Passenger Traffic:',
       caption = 'Visualization by Alex Elfering\nSource: DataSF',
       title = paste('Change in International Passenger Traffic at San Francisco International Airport: ', YearFilter, ' vs ', YearFilterPY, sep = '')) +
  theme(plot.title = element_text(face = 'bold', size = 22, family = 'Franklin Gothic Book'),
        plot.subtitle = element_text(face = 'bold', size = 12, family = 'Franklin Gothic Book'),
        legend.position = 'none',
        legend.background=element_blank(),
        legend.text = element_text(size = 14, family = 'Franklin Gothic Book'),
        legend.title = element_text(size = 14, family = 'Franklin Gothic Book'),
        legend.key=element_blank(),
        plot.title.position = "plot",
        plot.caption.position =  "plot",
        plot.caption = element_text(size = 14, family = 'Franklin Gothic Book', color = 'gray'),
        axis.title = element_text(size = 14, family = 'Franklin Gothic Book'),
        axis.text = element_text(size = 14, family = 'Franklin Gothic Book', color = '#969696'),
        axis.text.x.bottom = element_text(size = 14, family = 'Franklin Gothic Book', color = 'black'),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = ggplot2::element_text(size = 14, hjust = 0, face = 'bold', color = 'black', family = 'Franklin Gothic Book'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.y = element_line(colour = "#f0f0f0", linetype = "dashed"),
        panel.grid.major.x = element_blank()) 

SFOWaterfallChart

ggsave(SFOWaterfallChart,
       file = 'SFO Waterfall.png',
       width = 20,
       height = 10,
       units = 'in')
