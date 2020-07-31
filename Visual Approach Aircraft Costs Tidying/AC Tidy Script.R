library(readxl)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)

setwd("~/Documents/GitHub/Airport-Analysis/Visual Approach Aircraft Costs Tidying")

#### wrangling the data ####
files <- list.files(path = "~/Documents/GitHub/Airport-Analysis/Visual Approach Aircraft Costs Tidying/Data", pattern = "*.xls", full.names = T)
airline.list <- sapply(files, read_excel, simplify=FALSE)

#### functions ####
tidy_xl_vs <- function(df) {
  # This function:
  #   - identifies the year of the excel sheet and creates a variable for a column below
  #   - cleans the overall excel spreadsheet and removes excess NA rows and columns
  
  
  # identifying years and creating a column for them
  years <- c(2000:2020)
  y  <- NULL
  for (i in years) {
    year <- ifelse(grepl(i, colnames(df)[1]), i, NA)
    y <- as.data.frame(rbind(y, year))
  }
  
  year <- as.integer(dplyr::filter(y, !is.na(V1)))
  
  # removing unnecessary columns and rows
  rate <- as.character(df[2,1])
  airline <- as.character(df[2,2])
  
mark1 <- df %>%
  filter(!is.na(`...2`)) %>%
  mutate(Airline = airline,
         Frequency = rate)

colnames(mark1)[1] <- "Metric"
colnames(mark1)[2] <- "Total"
mark2 <- mark1[-1, ]
mark2$Period <- year

return(mark2)}
identify.equipment <- function(df){
  # This function:
  #   - identifies the airline's equipment (aircraft) being analyzed
  #   - also flags 'all' if it is an overall cost
  #   - also runs substr on the airline column to return the airline's IATA code
  
  # identify the airline's equipment
  df$Equipment <- ifelse(grepl('All', df$Airline), 'All', NA)
  
  # identify the airline
  df$Airline <- substr(df$Airline, 1, 2)
  
  return(df)
}

#### putting the functions to work and creating the final data frame ####
apply.tidy.list <- lapply(airline.list, tidy_xl_vs)
lapply(apply.tidy.list, identify.equipment)

airline.cost.df <- as.data.frame(do.call(rbind.data.frame, apply.tidy.list))