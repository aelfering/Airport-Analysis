library(glue)
library(tidyverse)
library(lubridate)
library(tabulizer)

# establish variables
years <- 2003:2021

date_df <- tibble(Month = month.name,
                  MonthInteger = 1:12)

ATHNames <- glue('{years} passengers.pdf')

# this for loop cycles through each pdf, converts it to a data frame, and removes unneded rows and columns before storing in a list
# the 2021 pdf has different rows selected due to additional columns added
ATHList <- list()
for(i in ATHNames){
  
  print(i)
  
  year_sub <- as.numeric(substr(i, 1, 4))
  
  Extractpdf <- extract_tables(i, pages = 1)
  
  Extract2003df <- as.data.frame(Extractpdf) %>%
    mutate(PrimaryYear = year_sub)
  
  if(Extract2003df$PrimaryYear <= 2020){
    
    NewDf <- Extract2003df %>%
      select(X1,
             X2,
             X3,
             X5,
             X6,
             X8,
             X9,
             PrimaryYear) %>%
      filter(X1 != '',
             X5 != '',
             X1 != 'Total Year',
             X1 != 'Total',
             X1 != 'Total year',
             X1 != 'Year-to-date') %>%
      rename(Month = X1,
             DomesticCY = X2,
             DomesticPY = X3,
             InternationalCY = X5,
             InternationalPY = X6,
             TotalCY = X8,
             TotalPY = X9)
    
  } else if (Extract2003df$PrimaryYear > 2020) {
    
    NewDf <- Extract2003df %>%
      select(X1,
             X2,
             X3,
             X6,
             X7,
             X10,
             X11,
             PrimaryYear) %>%
      filter(X1 != '',
             X6 != '',
             X1 != 'Total Year',
             X1 != 'Total',
             X1 != 'Total year',
             X1 != 'Year-to-date') %>%
      rename(Month = X1,
             DomesticCY = X2,
             DomesticPY = X3,
             InternationalCY = X6,
             InternationalPY = X7,
             TotalCY = X10,
             TotalPY = X11)
    
  }
  
  ATHList[[i]] <- NewDf
}

# bind the list elements together into one data frame
# also create a date column from lubridate using the year, month, and a 1 for the date column
AthenCleanData <- bind_rows(ATHList) %>%
  left_join(date_df) %>%
  mutate(Date = ymd(paste(PrimaryYear, MonthInteger, 1, sep = '-'))) %>%
  select(-Month) %>%
  pivot_longer(cols = -c('Date', 'MonthInteger', 'PrimaryYear'),
               names_to = 'Measure',
               values_to = 'Passengers') %>%
  mutate(Passengers = gsub('\\.', '', Passengers),
         Passengers = gsub('\\,', '', Passengers),
         Passengers = as.numeric(Passengers))


# visualizations
VizAuthor <- 'Visualization by Alex Elfering'
ATHSource <- 'Source: Athens International Airport S.A'
FullCaption <- paste0(VizAuthor, '\n', ATHSource)

AthensPaxFlown <- AthenCleanData %>%
  filter(Measure == 'TotalCY') %>%
  mutate(RollingPax = rollapplyr(Passengers, 12, sum, partial = TRUE)) %>%
  filter(row_number()  > 12) %>%
  ggplot() + 
  geom_line(mapping = aes(x = Date,
                          y = RollingPax,
                          group = year(Date)),
            color = '#2b8cbe',
            size = 1) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(breaks = seq.Date(as.Date("2004-01-01"), as.Date("2021-01-01"), by = '1 year'),
               labels = substr(2004:2021, 3, 4) ) +
  labs(title = 'Passenger Traffic at Athens International Airport',
       subtitle = 'Total passengers; rolling 12 months',
       x = '',
       y = '',
       caption = FullCaption) +
  theme(plot.title = element_text(face = 'bold', size = 14, family = 'Arial'),
        plot.subtitle = element_text(face = 'bold', size = 12, family = 'Arial'),
        legend.position = 'none',
        legend.background=element_blank(),
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

ggsave(AthensPaxFlown, file = 'Athens Pax Flown.png', width = 10, height = 4, units = 'in')


AthensDomesticInternational <- AthenCleanData %>%
  filter(Measure %in% c('DomesticCY', 'InternationalCY')) %>%
  group_by(Date) %>%
  mutate(Total = sum(Passengers)) %>%
  ungroup() %>%
  mutate(Share = Passengers/Total) %>%
  group_by(Measure) %>%
  mutate(RollPassengers = rollapplyr(Passengers, 12, sum, partial = TRUE),
         RollTotal = rollapplyr(Total, 12, sum, partial = TRUE),
         RollShare = RollPassengers/RollTotal) %>%
  filter(row_number() > 12) %>%
  ungroup() %>%
  ggplot() +
  geom_line(mapping = aes(x = Date,
                          y = RollShare,
                          color = Measure),
            size = 1) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c('steelblue',
                                'darkorange'),
                     labels = c('Domestic',
                                'International')) +
  scale_x_date(breaks = seq.Date(as.Date("2004-01-01"), as.Date("2021-01-01"), by = '1 year'),
               labels = substr(2004:2021, 3, 4) ) +
  labs(title = 'Percent of Traffic Traveling Domestic or International at Athens International Airport',
       subtitle = 'Percent of total passengers; rolling 12 months',
       color = '',
       x = '',
       y = '',
       caption = FullCaption) +
  theme(plot.title = element_text(face = 'bold', size = 14, family = 'Arial'),
        plot.subtitle = element_text(face = 'bold', size = 12, family = 'Arial'),
        legend.position = 'top',
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.text = element_text(size = 12, family = 'Arial'),
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

ggsave(AthensDomesticInternational, file = 'Athens Percent Domestic International.png', width = 10, height = 4, units = 'in')





