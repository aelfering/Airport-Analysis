# Tidying data from Visual Approach
# https://data.visualapproach.io/T100

#### Package Loading  ####
list.of.packages <- c("ggplot2", 
                      "dplyr", 
                      'tidyverse', 
                      'tidyr')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidyr)

# Load the dataset
visual_approach_pivot <- read.csv('Data-10.csv')

# Generating years + X to identify later in the dataframe
year.x <- paste('X', 1990:2020, sep = '')

clean_vs_pivot <- visual_approach_pivot %>%
  slice(-2) %>%
  # Pivot all of the date columns into rows save for Origin and Destination
  pivot_longer(-c('Origin', 'Destination')) %>%
  filter(Destination != '') %>%
  ungroup()

missing.years.filled <- clean_vs_pivot %>%
  # Return columns that contain years
  mutate(Contains.Year = ifelse(grepl(paste(year.x, collapse = "|"), name), str_sub(name,-4,-1), NA)) %>%
  # fill missing years in the year column
  fill(Contains.Year) %>%
  # rearrange the dataframe and update column data types
  select(Year = Contains.Year,
         Origin,
         Destination,
         Passengers = value) %>%
  mutate(Passengers = as.integer(Passengers),
         Year = as.integer(Year)) %>%
  group_by(Year,
           Origin,
           Destination) %>%
  mutate(Month = row_number()) %>%
  ungroup() %>%
  mutate(Date = as.Date(paste(Year, Month, '01', sep = '-')))
  
# Visualizations!

options(scipen = 999)

missing.years.filled %>%
  replace(is.na(.), 0) %>%
  group_by(Date,
           Origin) %>%
  summarise(Passengers = sum(Passengers)) %>%
  ungroup() %>%
  ggplot() +
  geom_line(mapping = aes(x = Date, 
                          y = Passengers, 
                          group = Origin,
                          color = Origin),
            size = 1) +
  labs(title = 'Monthly Passengers Totals for MCI, OMA and DSM',
       subtitle = 'January 2010 - April 2020'
       caption = 'Visualization by Alex Elfering\nSource: Visual Approach T-100 Pivot, Bureau of Transportation Statistics') +
  theme(legend.position = 'top')

