library(tidyr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidylog)
library(blscrapeR)
library(reactable)

setwd("~/Documents/GitHub/Airport-Analysis/Financial Analysis")

operating_expenses <- read.csv('Operating Expenses.csv')
pnl_statements <- read.csv('PnL Data.csv')

####  Adjust for inflation  ####
yearly_cpi <- inflation_adjust(2020) %>%
  distinct(year,
           avg_cpi) %>%
  mutate(year = as.integer(year))

# Return the most recent average CPI
current_cpi <- yearly_cpi %>%
  filter(year == max(year)) %>%
  select(avg_cpi)

current_cpi_int <- as.numeric(current_cpi)
####  Cleaning Operating Expenses ####

# Airline filter
CARRIER_NM <- 'WN'
YEAR_INT <- 2019
QUARTERS <- c(1, 2, 3, 4)
PY_YEAR <- YEAR_INT-1
  
# Overall Operating Expenses by Airline, Year, and Expense Group
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

# Visualizing the percent of total expenses by group
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

salaries <- c("SALARIES_MGT", "SALARIES_FLIGHT", "SALARIES_MAINT", "SALARIES_TRAFFIC", "SALARIES_OTHER")
benefits <- c("BENEFITS_PERSONNEL", "BENEFITS_PENSIONS", "BENEFITS_PAYROLL", "BENEFITS")
materials_purchased <- c("AIRCRAFT_FUEL", "MAINT_MATERIAL", "FOOD", "OTHER_MATERIALS")
services_purchased <- c("ADVERTISING", "COMMUNICATION", "INSURANCE", "OUTSIDE_EQUIP", "COMMISIONS_PAX", "COMMISSIONS_CARGO", "OTHER_SERVICES")
columns_to_not_select <- c("AIRLINE_ID", "UNIQUE_CARRIER", "UNIQUE_CARRIER_NAME", "OP_EXPENSE", "UNIQUE_CARRIER_ENTITY", "REGION", "CARRIER_GROUP_NEW", "CARRIER_GROUP", "QUARTER", "X",
                           "SALARIES_BENEFITS", "SALARIES", "BENEFITS", "MATERIALS_TOTAL", "SERVICES_TOTAL")

py_op_expenses <- operating_expenses %>%
  filter(CARRIER == CARRIER_NM, 
         QUARTER %in% QUARTERS,
         YEAR == PY_YEAR) %>%
  replace(is.na(.), 0) %>%
  select(-columns_to_not_select) %>%
  group_by(CARRIER, 
           CARRIER_NAME, 
           YEAR) %>%
  summarise_all(sum) %>%
  ungroup() %>%
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
  mutate(SUB.METRICS = gsub('\\_', ' ', SUB.METRICS)) %>%
  group_by(METRICS) %>%
  mutate(TOTAL = sum(SUB.AMOUNT)) %>%
  ungroup() %>%
  mutate(PCT.TOTAL = SUB.AMOUNT/TOTAL,
         GRAND.TOTAL = sum(SUB.AMOUNT),
         PCT.GRAND = SUB.AMOUNT/GRAND.TOTAL) %>%
  select(METRICS,
         SUB.METRICS,
         PY_AMOUNT = SUB.AMOUNT,
         PY_PCT.GRAND = PCT.GRAND,
         PY_PCT.TOTAL = PCT.TOTAL)

metrics.sub.metrics <- operating_expenses %>%
  filter(CARRIER == CARRIER_NM, 
         QUARTER %in% QUARTERS,
         YEAR == YEAR_INT) %>%
  replace(is.na(.), 0) %>%
  select(-columns_to_not_select) %>%
  group_by(CARRIER, 
           CARRIER_NAME, 
           YEAR) %>%
  summarise_all(sum) %>%
  ungroup() %>%
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
  mutate(SUB.METRICS = gsub('\\_', ' ', SUB.METRICS)) %>%
  group_by(CARRIER, METRICS) %>%
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
  full_join(py_op_expenses, by = c('METRICS' = 'METRICS', 'SUB.METRICS' = 'SUB.METRICS')) %>%
  mutate(YOY = (AMOUNT-PY_AMOUNT)/PY_AMOUNT) %>%
  filter(!is.nan(YOY)) %>%
  select(METRICS,
         SUB.METRICS,
         AMOUNT,
         PY_AMOUNT,
         YOY,
         PCT.TOTAL,
         PCT.GRAND)

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
    if (value > 0)                            paste0("+", round(value, 4)*100, '%') 
    else if (value <= 0)                      paste0(round(value, 4)*100, '%')
    else if (is.null(value))                   '-'
}

yoy_pct(NA)

knockout_pct_color <- make_color_pal(c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"), bias = 2)

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
                             maxWidth = 200),
            SUB.METRICS = colDef(maxWidth = 200,
                                 align = 'right'),
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
              # Calculate the aggregate Avg.Price as `sum(Price) / sum(Units)`
              aggregate = JS("function(values, rows) {
              var amount = 0
              var py_amount = 0
              rows.forEach(function(row) {
              amount += row['AMOUNT']
              py_amount += row['PY_AMOUNT']
              })
              return (amount-py_amount) / py_amount
                             }"),
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

####  Cleaning PnL  ####
head(pnl_statements, 3)

airlines <- c('AA', 'AS', 'G4', 'HP', 'UA', 'US', 'DL', 'TW', 'WN', 'VX', 'B6', 'NW')

airline_pnl <- pnl_statements %>%
  replace(is.na(.), 0) %>%
  filter(CARRIER %in% airlines) %>%
  group_by(YEAR,
           CARRIER) %>%
  summarise(OP_EXPENSES = sum(OP_EXPENSES))




