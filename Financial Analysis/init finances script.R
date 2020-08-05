library(tidyr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidylog)
library(blscrapeR)

library(ggvoronoi)
library(treemap)
library(d3treeR)

install.packages('d3treeR')

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
  filter(CARRIER %in% airlines) %>%
  group_by(CARRIER,
           YEAR) %>%
  summarise(SALARIES_BENEFITS = sum(SALARIES_BENEFITS),
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
  geom_step(size = 1) + 
  facet_wrap(~CARRIER)


# 
colnames(operating_expenses)
str(operating_expenses)

salaries_benefits <- c("SALARIES_MGT", "SALARIES_FLIGHT", "SALARIES_MAINT", "SALARIES_TRAFFIC", "SALARIES_OTHER", "BENEFITS_PERSONNEL", "BENEFITS_PENSIONS", "BENEFITS_PAYROLL", "BENEFITS")
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
  mutate(METRICS = ifelse(SUB.METRICS %in% salaries_benefits, "SALARIES_BENEFITS", NA),
         METRICS = ifelse(SUB.METRICS %in% materials_purchased, "MATERIALS_TOTAL", METRICS),
         METRICS = ifelse(SUB.METRICS %in% services_purchased, "SERVICES_TOTAL", METRICS),
         METRICS = ifelse(SUB.METRICS == "SERVICES_TOTAL", "SERVICES_TOTAL", METRICS),
         METRICS = ifelse(SUB.METRICS == "LANDING_FEES", "LANDING_FEES", METRICS),
         METRICS = ifelse(SUB.METRICS == "RENTALS", "RENTALS", METRICS),
         METRICS = ifelse(SUB.METRICS == "DEPRECIATION", "DEPRECIATION", METRICS),
         METRICS = ifelse(SUB.METRICS == "AMORTIZATION", "AMORTIZATION", METRICS),
         METRICS = ifelse(SUB.METRICS == "OTHER", "OTHER", METRICS),
         METRICS = ifelse(SUB.METRICS == "TRANS_EXPENSE", "TRANS_EXPENSE", METRICS))

# basic treemap
library(htmltools)

p <- treemap(metrics.sub.metrics,
             index=c("METRICS","SUB.METRICS"),
             vSize="SUB.AMOUNT",
             type="index",
             palette = "Set2",
             bg.labels=c("white"),
             align.labels=list(
               c("center", "center"), 
               c("center", "center")
               ),                                   # Where to place labels in the rectangle?
             overlap.labels=0.5,                      # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
             inflate.labels=F,                        # If true, labels are bigger when rectangle is bigger.
             )  

# make it interactive ("rootname" becomes the title of the plot):
browsable(
  tagList(
    tags$head(
      tags$style('text.label{font-size: 12px !important}')
    ),
    d3tree(p,
           rootname = "Components of an Airline' Operating Expenses" )
    ,rootname = "Visualization by Alex Elfering Source: Bureau of Transportation Statistics")
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




