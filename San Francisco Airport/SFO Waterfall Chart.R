# San Francisco International Airport waterfall chart
# examines the change in passenger traffic among international geo regions

# created:  20 July 2021


# load the libraries  ----
library(tidyverse)
library(tidylog)
library(ggpubr)
library(ggplot2)
library(lubridate)
library(glue)
library(extrafont)

# load the data  ----
SFOPax <- read.csv('SFO Passengers.csv')

# data cleaning  ----
head(SFOPax)

SFOPaxClean <- SFOPax %>%
  mutate(Year =(substr(Activity.Period, 1, 4)),
         Month = (substr(Activity.Period, 5, 6)),
         Date = ymd(paste(Year, Month, 1, sep = '-') )) %>%
  select(-Activity.Period,
         -Month)
  
# find year-over-year passenger change by geo region  ----
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
         GEO.Region != 'US',
         !is.na(PY))

# find overall year-over-year change  ----
SFOYearPax <- SFOYOYRegion %>%
  group_by(Year) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  ungroup() %>%
  mutate(YOY = Diff/PY)

# variables for testing  ----
YearFilter <- 2020
YearFilterPY <- YearFilter-1

# additional charts for context  ----
YearlyAnnualPax <- SFOYearPax %>%
  filter(Year %in% c(YearFilter, YearFilterPY))

BarInit <- ggplot() + 
  geom_bar(YearlyAnnualPax,
           mapping = aes(x = Year,
                         y = Pax,
                         fill = Year),
           width = 0.5,
           stat = 'identity',
           position = 'identity') +
  scale_fill_manual(values = c('#bdbdbd',
                               '#636363')) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = scales::comma,
                     expand = expansion(mult = c(0, .05))) +
  labs(x = '',
       y = '',
       #color = 'Passenger Traffic:',
       #title = paste('San Francisco International Airport YOY International Passenger Traffic by Region'),
       title = 'Annual Passenger Traffic') +
  theme(plot.title = element_text(face = 'bold', size = 14, family = 'Franklin Gothic Book'),
        plot.subtitle = element_text(face = 'bold', size = 12, family = 'Franklin Gothic Book'),
        legend.position = 'none',
        legend.background=element_blank(),
        legend.text = element_text(size = 12, family = 'Franklin Gothic Book'),
        legend.title = element_text(size = 12, family = 'Franklin Gothic Book'),
        legend.key=element_blank(),
        plot.title.position = "plot",
        plot.caption.position =  "plot",
        plot.caption = element_text(size = 12, family = 'Franklin Gothic Book'),
        axis.title = element_text(size = 12, family = 'Franklin Gothic Book'),
        axis.text = element_text(size = 12, family = 'Franklin Gothic Book', color = '#969696'),
        axis.text.x.bottom = element_text(size = 12, family = 'Franklin Gothic Book', color = 'black'),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'black', family = 'Franklin Gothic Book'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.y = element_line(colour = "#c1c1c1", linetype = "dashed"),
        panel.grid.major.x = element_blank()) 

YearRegionPax <- SFOYOYRegion %>%
  filter(Year %in% c(YearFilter,
                     YearFilterPY))

GroupedInit <- ggplot(YearRegionPax, 
                      aes(factor(GEO.Region), 
                          Pax, 
                          fill = Year),
                      width = 0.5) + 
  geom_bar(stat="identity", 
           position = "dodge") + 
  scale_fill_manual(values = c('#bdbdbd',
                               '#636363')) +
  scale_y_continuous(labels = scales::comma,
                     expand = expansion(mult = c(0, .05))) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  labs(title = 'Pax Traffic by Region',
       x = '',
       y = '') +
  theme(plot.title = element_text(face = 'bold', size = 14, family = 'Franklin Gothic Book'),
        plot.subtitle = element_text(face = 'bold', size = 12, family = 'Franklin Gothic Book'),
        legend.position = 'none',
        legend.background=element_blank(),
        legend.text = element_text(size = 12, family = 'Franklin Gothic Book'),
        legend.title = element_text(size = 12, family = 'Franklin Gothic Book'),
        legend.key=element_blank(),
        plot.title.position = "plot",
        plot.caption.position =  "plot",
        plot.caption = element_text(size = 12, family = 'Franklin Gothic Book'),
        axis.title = element_text(size = 12, family = 'Franklin Gothic Book'),
        axis.text = element_text(size = 12, family = 'Franklin Gothic Book', color = '#969696'),
        axis.text.x.bottom = element_text(size = 12, family = 'Franklin Gothic Book', color = 'black'),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'black', family = 'Franklin Gothic Book'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.y = element_line(colour = "#c1c1c1", linetype = "dashed"),
        panel.grid.major.x = element_blank()) 

# return pax traffic numbers from YearFilter  ----
SFOYearPaxFilter <- dplyr::filter(SFOYearPax, Year == YearFilter)

PYPaxDF <- tibble(GEO.Region = as.character(YearFilterPY),
                  Pax = as.numeric(SFOYearPaxFilter[,3]))

CYPaxDF <- tibble(GEO.Region = as.character(YearFilter),
                  Pax = as.numeric(SFOYearPaxFilter[,2]))

YearRegionSelect <- SFOYOYRegion %>%
  filter(Year == YearFilter) %>%
  select(GEO.Region,
         Pax = Diff) %>%
  arrange(desc(Pax))

# create the waterfall DF  ----
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
               mapping = aes(x = seq(1.7, nrow(WaterfallDF)-1.3, 1), 
                             y = Beg, 
                             xend = seq(1.7, nrow(WaterfallDF)-1.3, 1), 
                             yend = End),
               arrow = arrow(type = "open",
                             length = unit(0.02, "npc")),
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
       #title = paste('San Francisco International Airport YOY International Passenger Traffic by Region'),
       title = paste('Change in international passenger traffic ', YearFilter, ' vs ', YearFilterPY, sep = '')) +
  theme(plot.title = element_text(face = 'bold', size = 14, family = 'Franklin Gothic Book'),
        plot.subtitle = element_text(face = 'bold', size = 12, family = 'Franklin Gothic Book'),
        legend.position = 'none',
        legend.background=element_blank(),
        legend.text = element_text(size = 12, family = 'Franklin Gothic Book'),
        legend.title = element_text(size = 12, family = 'Franklin Gothic Book'),
        legend.key=element_blank(),
        plot.title.position = "plot",
        plot.caption.position =  "plot",
        plot.caption = element_text(size = 12, family = 'Franklin Gothic Book'),
        axis.title = element_text(size = 12, family = 'Franklin Gothic Book'),
        axis.text = element_text(size = 12, family = 'Franklin Gothic Book', color = '#969696'),
        axis.text.x.bottom = element_text(size = 12, family = 'Franklin Gothic Book', color = 'black'),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'black', family = 'Franklin Gothic Book'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.y = element_line(colour = "#c1c1c1", linetype = "dashed"),
        panel.grid.major.x = element_blank()) 

# put the final visualizations together  ----
Group1 <- ggarrange(BarInit,
                    GroupedInit,
                    widths = c(0.7, 2))

Group2 <- ggarrange(Group1,
                    SFOWaterfallChart,
                    nrow = 2,
                    heights = c(0.7, 1))

FinalVisualization <- annotate_figure(Group2, 
                                      top = text_grob(paste("International Passenger Traffic at\nSan Francisco International Airport ", YearFilter, '-', YearFilterPY, sep = ''), 
                                                      color = "black", 
                                                      family = 'Franklin Gothic Book',
                                                      face = "bold", 
                                                      size = 22),
                                      bottom = text_grob("Visualization by Alex Elfering\nSource: DataSF", 
                                                         color = "gray", 
                                                         family = 'Franklin Gothic Book',
                                                         #face = "bold", 
                                                         size = 10))

ggsave(FinalVisualization, file = 'FinalVisualization.png', width = 10, height = 10, units = 'in')
FinalVisualization


