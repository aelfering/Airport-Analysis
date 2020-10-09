library(ggplot2)  
library(shiny)
library(htmltools)
library(shinydashboard)
library(shinythemes) 
library(dplyr) 
library(lubridate)
library(tidyr)
library(tidyverse)
library(scales)
library(data.table)
library(zoo)
library(ggrepel)
library(rsconnect)

ak_flights <- read.csv('top_alaska_airport_flights.csv')

ui <- shinyUI(fluidPage(  
    titlePanel("Percent of Domestic Flights Cancelled Among Major Alaska Airports"),  
    sidebarLayout(  
        sidebarPanel(
            selectInput("airport", "Select an Airport:",
                        unique(ak_flights$ORIGIN)),
            sliderInput("range", "Selecting a Running Sum Variable:",
                        min = 7, max = 30,
                        value = 1),
            dateInput('date',
                      label = 'Select a Date:',
                      min = as.Date('2020-01-01'),
                      max = max(as.Date(ak_flights$FL_DATE)),
                      value = max(as.Date(ak_flights$FL_DATE))),
            width=2),
        mainPanel(
            plotOutput("Plot", width = "100%")  
        )  
    )  
))

server <- shinyServer(function(input, output) {  
    output$Plot <- renderPlot({  
        
        selection_date <- input$date
        begin_day <- selection_date-input$range
        
        all_stats <- ak_flights %>%
            select(-c(X)) %>%
            mutate(OP_FLIGHTS = FLIGHTS-CANCELLED) %>%
            group_by(ORIGIN) %>%
            mutate(ROLLING_OP_FLIGHTS = rollapplyr(OP_FLIGHTS, input$range, sum, partial = TRUE),
                   ROLLING_FLIGHTS = rollapplyr(FLIGHTS, input$range, sum, partial = TRUE),
                   ROLLING_DELAYS = rollapplyr(DELAYS, input$range, sum, partial = TRUE),
                   ROLLING_CANCELLED = rollapplyr(CANCELLED, input$range, sum, partial = TRUE),
                   ROLLING_DEL_PCT = ROLLING_DELAYS/ROLLING_FLIGHTS) %>%
            ungroup() %>%
            mutate(ROLLING_PCT_CANCELLED = 1-(ROLLING_OP_FLIGHTS/ROLLING_FLIGHTS),
                   ROLLING_PCT_DELAYED = 1-(ROLLING_DELAYS/ROLLING_OP_FLIGHTS),
                   FL_DATE = as.Date(FL_DATE)) %>%
            select(FL_DATE,
                   ORIGIN,
                   ROLLING_FLIGHTS,
                   ROLLING_OP_FLIGHTS,
                   ROLLING_DELAYS,
                   ROLLING_CANCELLED,
                   ROLLING_PCT_DELAYED,
                   ROLLING_PCT_CANCELLED)
        
        flights_2020 <- all_stats %>%
            filter(FL_DATE >= as.Date('2020-01-01'),
                   ORIGIN == input$airport) %>%
            mutate(FL_DATE = as.Date(FL_DATE),
                   PREV_DATE = as.Date(FL_DATE-364)) %>%
            inner_join(all_stats,
                       by = c('PREV_DATE' = 'FL_DATE',
                              'ORIGIN' = 'ORIGIN')) %>%
            select(FL_DATE,
                   ORIGIN,
                   CY_ROLLING_CANCEL = ROLLING_PCT_CANCELLED.x,
                   PY_ROLLING_CANCEL = ROLLING_PCT_CANCELLED.y)
        
        filter_day <- dplyr::filter(flights_2020, FL_DATE == selection_date)
        
        can_rate <- paste(round(filter_day$CY_ROLLING_CANCEL, 4)*100, '%', sep = '')
        py_can_rate <- paste(round(filter_day$PY_ROLLING_CANCEL, 4)*100, '%', sep = '')
        airport_name <- filter_day$ORIGIN
        
        direction <- ifelse(can_rate > py_can_rate, 'up',
                            ifelse(can_rate < py_can_rate, 'down', 'same'))
        
        title_text <- paste(airport_name, ' Airport: ', can_rate, ' of Domestic Flights were Cancelled Between ', format(strptime(begin_day, format = "%Y-%m-%d"), "%b-%d"), ' and ', format(strptime(selection_date, format = "%Y-%m-%d"), "%b-%d"), sep = '')
        
        subtitle_text <- paste('This is ', direction, ' from ', py_can_rate, ' during the same time period last year. ', '\nBased on a ', input$range, ' day rolling sum of Operated Flights and Cancelled Flights.\n', sep = '')
        
        ggplot(flights_2020,
               aes(x = as.Date(FL_DATE),
                   y = CY_ROLLING_CANCEL)) +
            geom_hline(yintercept = 0,
                       color = '#c1c1c1') +
            geom_vline(xintercept = selection_date,
                       linetype = 'dashed',
                       color = '#c1c1c1') +
            geom_line(data = flights_2020,
                      mapping = aes(x = as.Date(FL_DATE),
                                    y = PY_ROLLING_CANCEL),
                      size = 1,
                      color = '#ffd198') +
            geom_line(data = subset(flights_2020, 
                                    FL_DATE >= begin_day & FL_DATE <= selection_date),
                      mapping = aes(x = as.Date(FL_DATE),
                                    y = PY_ROLLING_CANCEL),
                      color = '#ff6114',
                      size = 1) +
            geom_line(size = 1,
                      color = '#b8dfe8') +
            geom_line(data = subset(flights_2020, FL_DATE >= begin_day & FL_DATE <= selection_date),
                      mapping = aes(x = as.Date(FL_DATE),
                                    y = CY_ROLLING_CANCEL),
                      color = '#00a7c2',
                      size = 1) +
            geom_point(data = subset(flights_2020, FL_DATE == selection_date),
                       mapping = aes(x = as.Date(FL_DATE),
                                     y = PY_ROLLING_CANCEL),
                       color = '#ff6114',
                       size = 2) +
            geom_point(data = subset(flights_2020, FL_DATE == selection_date),
                       mapping = aes(x = as.Date(FL_DATE),
                                     y = CY_ROLLING_CANCEL),
                       color = '#00a7c2',
                       size = 2) +
            #geom_point(data = subset(all_stats, FL_DATE == selection_date),
            #           size = 2) +
            #geom_ribbon(data = subset(all_stats, FL_DATE >= begin_day & FL_DATE <= selection_date), 
            #            aes(ymin = 0, ymax = ROLLING_PCT_CANCELLED), fill="#00429d", alpha=0.2) +
            geom_label_repel(data = subset(flights_2020, FL_DATE == selection_date),
                             mapping = aes(y = CY_ROLLING_CANCEL,
                                           label = paste('This Year: ', can_rate, sep = '')),                   
                             box.padding = 4, 
                             color = '#00a7c2') +
            geom_label_repel(data = subset(flights_2020, FL_DATE == selection_date),
                             mapping = aes(y = PY_ROLLING_CANCEL,
                                           label = paste('Last Year: ', py_can_rate, sep = '')),
                             box.padding = 0.5, 
                             color = '#ff6114') +
            scale_y_continuous(labels = percent) +
            labs(x = '',
                 y = '',
                 title = title_text,
                 subtitle = subtitle_text,
                 caption = 'Visualization by Alex Elfering    Source: Bureau of Transportation Statistics\nCancelled flights are calculated by dividing Operated Flights by Scheduled Flights and subtracting the result by 1.')  +
            theme(plot.title = element_text(face = 'bold', size = 18, family = 'Arial'),
                  legend.position = 'top',
                  legend.background=element_blank(),
                  legend.key=element_blank(),
                  legend.text = element_text(size = 12, family = 'Arial'),
                  legend.title = element_text(size = 12, family = 'Arial'),
                  plot.subtitle = element_text(size = 15, family = 'Arial'),
                  plot.caption = element_text(size = 12, family = 'Arial'),
                  axis.title = element_text(size = 12, family = 'Arial'),
                  axis.text = element_text(size = 12, family = 'Arial'),
                  strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'black', family = 'Arial'),
                  strip.background = element_rect(fill = NA),
                  panel.background = ggplot2::element_blank(),
                  axis.line = element_line(colour = "#222222", linetype = "solid"),
                  panel.grid.major.y = element_line(colour = "#c1c1c1", linetype = "dashed"),
                  panel.grid.major.x = element_blank()) 
        
        
    })
})

shinyApp(ui=ui, server=server)