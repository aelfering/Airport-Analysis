library(tidyr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidylog)
library(blscrapeR)

remotes::install_github("d3treeR/d3treeR")

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
airlines <- c('AA', 'AS', 'G4', 'HP', 'UA', 'US', 'DL', 'F9', 'TW', 'NK', 'WN', 'VX', 'B6', 'NW')

# Overall Operating Expenses by Airline, Year, and Expense Group
airline_op_exp <- operating_expenses %>%
  replace(is.na(.), 0) %>%
  filter(CARRIER == 'WN') %>%
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
  geom_hline(yintercept = 0, linetype = 'dashed') +
  geom_line(size = 1) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = 'What Components Make Up Airline Operating Expenses?',
       y = 'Percent of Total Operating Expenses',
       x = 'Year',
       caption = 'Visualization by Alex Elfering\nSource: Bureau of Transportation Statistics') +
  theme(legend.position = 'top')


# 
colnames(operating_expenses)
str(operating_expenses)

salaries <- c("SALARIES_MGT", "SALARIES_FLIGHT", "SALARIES_MAINT", "SALARIES_TRAFFIC", "SALARIES_OTHER")
benefits <- c("BENEFITS_PERSONNEL", "BENEFITS_PENSIONS", "BENEFITS_PAYROLL", "BENEFITS")
materials_purchased <- c("AIRCRAFT_FUEL", "MAINT_MATERIAL", "FOOD", "OTHER_MATERIALS")
services_purchased <- c("ADVERTISING", "COMMUNICATION", "INSURANCE", "OUTSIDE_EQUIP", "COMMISIONS_PAX", "COMMISSIONS_CARGO", "OTHER_SERVICES")
columns_to_not_select <- c("AIRLINE_ID", "UNIQUE_CARRIER", "UNIQUE_CARRIER_NAME", "OP_EXPENSE", "UNIQUE_CARRIER_ENTITY", "REGION", "CARRIER_GROUP_NEW", "CARRIER_GROUP", "QUARTER", "X",
                           "SALARIES_BENEFITS", "SALARIES", "BENEFITS", "MATERIALS_TOTAL", "SERVICES_TOTAL")

metrics.sub.metrics <- operating_expenses %>%
  filter(CARRIER %in% airlines) %>%
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
  filter(CARRIER == 'UA', 
         YEAR == 2020) %>%
  group_by(METRICS) %>%
  mutate(TOTAL = sum(SUB.AMOUNT)) %>%
  ungroup() %>%
  mutate(PCT.TOTAL = SUB.AMOUNT/TOTAL) %>%
  select(METRICS,
         SUB.METRICS,
         AMOUNT = SUB.AMOUNT,
         PCT.TOTAL)

head(metrics.sub.metrics)

reactable(metrics.sub.metrics, 
          pagination = FALSE,
          outlined = TRUE,
          highlight = TRUE,
          striped = TRUE,
          resizable = TRUE,
          wrap = TRUE,
          defaultColDef = colDef(headerClass = "header", 
                                 align = "left"),
          style = list(fontFamily = 'Arial', 
                       fontSize = '14px'),
          theme = reactableTheme(
            headerStyle = list(
              "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
              "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
              borderColor = "#555"
            )),
          groupBy = c("METRICS"), 
          columns = list(
            METRICS = colDef(footer = "Total"),
            AMOUNT = colDef(aggregate = "sum", 
                            format = colFormat(currency = "USD", 
                                               separators = TRUE, 
                                               digits = 2)),
            PCT.TOTAL = colDef(aggregate = "sum",
                               format = colFormat(percent = TRUE, digits = 2))
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




