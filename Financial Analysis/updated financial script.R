# Airline Quaterly Operating Expense Dashboard
# Script by Alex Elfering

# Source: Bureau of Transportation Statistics, Air Carrier Financial: Schedule P-6
# Link: https://www.transtats.bts.gov/DL_SelectFields.asp

####  Load libraries, Environment and data sources ####
list.of.packages <- c("tidyr", 
                      "tidyverse", 
                      'ggplot2', 
                      'dplyr', 
                      'tidylog', 
                      'reactable',
                      'shiny',
                      'shinythemes',
                      'shinydashboard')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidylog)
library(reactable)
library(shiny)
library(shinythemes)
library(shinydashboard)

options(scipen = 999)

setwd("~/Documents/GitHub/Airport-Analysis/Financial Analysis")

operating_expenses <- read.csv('Operating Expenses.csv')

####  Variables for Testing ####
CARRIER_NM <- 'G4'
YEAR_INT <- 2014
QUARTERS <- c(1)
PY_YEAR <- YEAR_INT-1

####  Data Cleaning ####

# This returns the percent of total operating expenses per year and by metric
# Filters for the airline and quarters selected in the shiny dashboard
airline_op_exp <- operating_expenses %>%
  replace(is.na(.), 0) %>%
  filter(CARRIER == CARRIER_NM,
         QUARTER %in% QUARTERS) %>%
  group_by(CARRIER,
           YEAR) %>%
  summarise(SALARIES = sum(SALARIES),
            BENEFITS = sum(BENEFITS),
            MATERIALS_TOTAL = sum(MATERIALS_TOTAL),
            SERVICES_TOTAL = sum(SERVICES_TOTAL),
            LANDING_FEES = sum(LANDING_FEES),
            RENTALS = sum(RENTALS),
            DEPRECIATION = sum(DEPRECIATION),
            AMORTIZATION = sum(AMORTIZATION),
            OTHER = sum(OTHER),
            TRANS_EXPENSE = sum(TRANS_EXPENSE)) %>%
  ungroup() %>%
  pivot_longer(-c(CARRIER, YEAR), 
               names_to = "METRICS", 
               values_to = "AMOUNT") %>% 
  group_by(CARRIER,
           YEAR) %>%
  mutate(TOTAL = sum(AMOUNT)) %>%
  ungroup() %>%
  mutate(PCT_TOTAL = AMOUNT/TOTAL)


## Next we need to break down each metric by their components
## The initial data breaks out columns in a cross tab, and we need to pivot and group the columns by metrics
salaries <- c("SALARIES_MGT", "SALARIES_FLIGHT", "SALARIES_MAINT", "SALARIES_TRAFFIC", "SALARIES_OTHER")
benefits <- c("BENEFITS_PERSONNEL", "BENEFITS_PENSIONS", "BENEFITS_PAYROLL", "BENEFITS")
materials_purchased <- c("AIRCRAFT_FUEL", "MAINT_MATERIAL", "FOOD", "OTHER_MATERIALS")
services_purchased <- c("ADVERTISING", "COMMUNICATION", "INSURANCE", "OUTSIDE_EQUIP", "COMMISIONS_PAX", "COMMISSIONS_CARGO", "OTHER_SERVICES")
columns_to_not_select <- c("AIRLINE_ID", "UNIQUE_CARRIER", "UNIQUE_CARRIER_NAME", "OP_EXPENSE", "UNIQUE_CARRIER_ENTITY", "REGION", "CARRIER_GROUP_NEW", "CARRIER_GROUP", "QUARTER", "X",
                           "SALARIES_BENEFITS", "SALARIES", "BENEFITS", "MATERIALS_TOTAL", "SERVICES_TOTAL")

# This data frame returns metric and sub metric operating expenses for a particular airline, the prior year (based on current year selected) and quarters
py_op_expenses <- operating_expenses %>%
  # Filtering the data and removing NA values
  filter(CARRIER == CARRIER_NM, 
         QUARTER %in% QUARTERS,
         YEAR == PY_YEAR) %>%
  replace(is.na(.), 0) %>%
  # Remove columns that are totals for respective metrics
  select(-columns_to_not_select) %>%
  # Group the data and summarise all numeric and integer columns
  group_by(CARRIER, 
           CARRIER_NAME, 
           YEAR) %>%
  summarise_all(sum) %>%
  ungroup() %>%
  # Pivot columns into sub metrics and group them by overall metrics (based on variables above)
  pivot_longer(-c(CARRIER, 
                  CARRIER_NAME, 
                  YEAR), 
               names_to = "SUB.METRICS", 
               values_to = "SUB.AMOUNT")  %>%
  mutate(METRICS = ifelse(SUB.METRICS %in% salaries, "Salaries", NA),
         METRICS = ifelse(SUB.METRICS %in% benefits, "Benefits", METRICS),
         METRICS = ifelse(SUB.METRICS %in% materials_purchased, "Total Materials", METRICS),
         METRICS = ifelse(SUB.METRICS %in% services_purchased, "Services Purchased", METRICS),
         METRICS = ifelse(SUB.METRICS == "SERVICES_TOTAL", "Total Services", METRICS),
         METRICS = ifelse(SUB.METRICS == "LANDING_FEES", "Landing Fees", METRICS),
         METRICS = ifelse(SUB.METRICS == "RENTALS", "Rentals", METRICS),
         METRICS = ifelse(SUB.METRICS == "DEPRECIATION", "Depreciation", METRICS),
         METRICS = ifelse(SUB.METRICS == "AMORTIZATION", "Amoritization", METRICS),
         METRICS = ifelse(SUB.METRICS == "OTHER", "Other", METRICS),
         METRICS = ifelse(SUB.METRICS == "TRANS_EXPENSE", "Trans Expense", METRICS)) %>%
  # Remove the '_' sign for better readability
  mutate(SUB.METRICS = gsub('\\_', ' ', SUB.METRICS)) %>%
  # Return the total and calculate the proportion of total operating expenses
  group_by(METRICS) %>%
  mutate(TOTAL = sum(SUB.AMOUNT)) %>%
  ungroup() %>%
  mutate(PCT.TOTAL = SUB.AMOUNT/TOTAL,
         GRAND.TOTAL = sum(SUB.AMOUNT),
         PCT.GRAND = SUB.AMOUNT/GRAND.TOTAL) %>%
  # The final select (we will join this to the current year data frame right below)
  select(METRICS,
         SUB.METRICS,
         PY_AMOUNT = SUB.AMOUNT,
         PY_PCT.GRAND = PCT.GRAND,
         PY_PCT.TOTAL = PCT.TOTAL)

metrics.sub.metrics <- operating_expenses %>%
  # Filtering the data and removing NA values
  filter(CARRIER == CARRIER_NM, 
         QUARTER %in% QUARTERS,
         YEAR == YEAR_INT) %>%
  replace(is.na(.), 0) %>%
  # Remove columns that are totals for respective metrics
  select(-columns_to_not_select) %>%
  group_by(CARRIER, 
           CARRIER_NAME, 
           YEAR) %>%
  # Group the data and summarise all numeric and integer columns
  summarise_all(sum) %>%
  ungroup() %>%
  # Pivot columns into sub metrics and group them by overall metrics (based on variables above)
  pivot_longer(-c(CARRIER, 
                  CARRIER_NAME, 
                  YEAR), 
               names_to = "SUB.METRICS", 
               values_to = "SUB.AMOUNT")  %>%
  mutate(METRICS = ifelse(SUB.METRICS %in% salaries, "Salaries", NA),
         METRICS = ifelse(SUB.METRICS %in% benefits, "Benefits", METRICS),
         METRICS = ifelse(SUB.METRICS %in% materials_purchased, "Total Materials", METRICS),
         METRICS = ifelse(SUB.METRICS %in% services_purchased, "Services Purchased", METRICS),
         METRICS = ifelse(SUB.METRICS == "SERVICES_TOTAL", "Total Services", METRICS),
         METRICS = ifelse(SUB.METRICS == "LANDING_FEES", "Landing Fees", METRICS),
         METRICS = ifelse(SUB.METRICS == "RENTALS", "Rentals", METRICS),
         METRICS = ifelse(SUB.METRICS == "DEPRECIATION", "Depreciation", METRICS),
         METRICS = ifelse(SUB.METRICS == "AMORTIZATION", "Amoritization", METRICS),
         METRICS = ifelse(SUB.METRICS == "OTHER", "Other", METRICS),
         METRICS = ifelse(SUB.METRICS == "TRANS_EXPENSE", "Trans Expense", METRICS)) %>%
  # Remove the '_' sign for better readability
  mutate(SUB.METRICS = gsub('\\_', ' ', SUB.METRICS)) %>%
  # Return the total and calculate the proportion of total operating expenses
  group_by(METRICS) %>%
  mutate(TOTAL = sum(SUB.AMOUNT)) %>%
  ungroup() %>%
  mutate(PCT.TOTAL = SUB.AMOUNT/TOTAL,
         GRAND.TOTAL = sum(SUB.AMOUNT),
         PCT.GRAND = SUB.AMOUNT/GRAND.TOTAL) %>%
  select(METRICS,
         SUB.METRICS,
         AMOUNT = SUB.AMOUNT,
         PCT.GRAND,
         PCT.TOTAL) %>%
  # Bring the prior year data frame in
  full_join(py_op_expenses, by = c('METRICS' = 'METRICS', 'SUB.METRICS' = 'SUB.METRICS')) %>%
  mutate(YOY = (AMOUNT-PY_AMOUNT)/PY_AMOUNT) %>%
  # Remove all rows that have NaN values
  filter(!is.nan(YOY)) %>%
  # The final select
  select(METRICS,
         SUB.METRICS,
         AMOUNT,
         PY_AMOUNT,
         YOY,
         PCT.TOTAL,
         PCT.GRAND)


####  Data Visualizations ####

# This visualization examines the percent of total operating expenses by metric and year
# Filters for airline and quarter
ggplot(subset(airline_op_exp, METRICS != 'OTHER'),
       aes(x = YEAR,
           y = PCT_TOTAL,
           group = METRICS,
           color = METRICS)) +
  geom_hline(yintercept = 0, 
             linetype = 'dashed') +
  geom_line(size = 1) +
  geom_vline(xintercept = YEAR_INT, 
             linetype = 'dashed') +
  scale_y_continuous(labels = scales::percent) +
  labs(title = 'What Components Make Up Airline Operating Expenses?',
       y = 'Percent of Total Operating Expenses',
       x = 'Year',
       caption = 'Visualization by Alex Elfering\nSource: Bureau of Transportation Statistics') +
  theme(legend.position = 'top')


####  Functions to Format the Shiny Table ####
##  The first three functions are courtesy of: 
#   https://glin.github.io/reactable/articles/womens-world-cup/womens-world-cup.html

knockout_column <- function(maxWidth = 70, class = NULL, ...) {
  colDef(
    cell = format_pct,
    maxWidth = maxWidth,
    class = paste("cell number", class),
    style = function(value) {
      # Lighter color for <1%
      if (value < 0.01) {
        list(color = "#111")
      } else {
        list(color = "#111", background = knockout_pct_color(value))
      }
    },
    ...
  )
}
format_pct <- function(value) {
  if (value == 0) "  \u2013 "    # en dash for 0%
  #else if (value == 1) "\u2713"  # checkmark for 100%
  else if (value < 0.01) " <1%"
  #else if (value > 0.99) ">99%"
  else formatC(paste0(round(value * 100, 2), "%"), width = 4)
}
make_color_pal <- function(colors, bias = 1) {
  get_color <- colorRamp(colors, bias = bias)
  function(x) rgb(get_color(x), maxColorValue = 255)
}
yoy_pct <- function(value){
  if (is.infinite(value) == TRUE)       "-"
  else if (value > 0)                   paste0("+", round(value, 4)*100, '%') 
  else if (value <= 0)                  paste0(round(value, 4)*100, '%')
}

knockout_pct_color <- make_color_pal(c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"), bias = 2)
####  Shiny Table ####
reactable(# Themes
  metrics.sub.metrics, 
  highlight = TRUE,
  wrap = TRUE,
  outlined = TRUE,
  defaultColDef = colDef(headerClass = "header", 
                         align = "left",
                         footerStyle = list(fontWeight = "bold")),
  style = list(fontFamily = 'Arial', 
               fontSize = '14px'),
  
  # Column formatting
  groupBy = c("METRICS"), 
  columns = list(
    METRICS = colDef(footer = "Total Operating Expenses",
                     maxWidth = 200,
                     name = 'Metric'),
    SUB.METRICS = colDef(maxWidth = 200,
                         align = 'right',
                         name = 'Sub-Metric'),
    AMOUNT = colDef(aggregate = "sum", 
                    maxWidth = 200,
                    name = 'Operating Expenses',
                    footer = function(values) paste('$', 
                                                    formatC(sum(values), format="f", big.mark = ",", digits=2), 
                                                    sep = ''),
                    align = 'left',
                    format = colFormat(currency = "USD", 
                                       separators = TRUE, 
                                       digits = 2)),
    PY_AMOUNT = colDef(aggregate = "sum", 
                       maxWidth = 200,
                       name = 'LY Operating Expenses',
                       footer = function(values) paste('$', 
                                                       formatC(sum(values), format="f", big.mark = ",", digits=2), 
                                                       sep = ''),
                       align = 'left',
                       format = colFormat(currency = "USD", 
                                          separators = TRUE, 
                                          digits = 2)),
    YOY = colDef(
    aggregate = JS("function(values, rows) {
                    var amount = 0
                    var py_amount = 0
                    rows.forEach(function(row) {
                    amount += row['AMOUNT']
                    py_amount += row['PY_AMOUNT']
                    })
              
                    if (((amount-py_amount) / py_amount) == Number.POSITIVE_INFINITY){
                    return '-'
                    } else {
                    return (amount-py_amount) / py_amount
                    }       
                             }"),
    #footer = ,
    format = colFormat(digits = 2,
                         percent = TRUE),
    cell = yoy_pct,
    style = function(value) {
        color <- if (value > 0) {
          "#008000"
        } else if (value < 0) {
          "#e00000"
        }
        list(fontWeight = 600, color = color)
      }
    ),
    PCT.GRAND = knockout_column(name = "Percent of Total Expenses", 
                                align = 'left',
                                format = colFormat(digits = 2,
                                                   percent = TRUE),
                                aggregate = 'sum',
                                maxWidth = 200),
    PCT.TOTAL = knockout_column(name = "Percent of Total Metric", 
                                align = 'left',
                                format = colFormat(digits = 2,
                                                   percent = TRUE),
                                maxWidth = 170),
    rowStyle = JS("function(rowInfo) {
          if (rowInfo.level > 0) {
          return { borderLeft: '2px solid #000000' }
          } else {
          return { borderLeft: '1px solid transparent' }
          }
                        }")
  )
)




















