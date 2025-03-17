################################################################################
# 7.2 Histograms - Relationship Between Expenditure and Income
################################################################################
# Author: Andrés Gutiérrez, Stalyn Guerrero
# 
# Description:
# This section analyzes the relationship between per capita household expenditure 
# and income using scatter plots. The data is weighted using survey weights to 
# ensure representativity. Both raw and log-transformed values are visualized.
#
# Data Source:
# https:/microdata.worldbank.org/index.php/catalog/3823/data-dictionary
# Version:  1
# Date    :  13-03-2025
################################################################################

#------------------------------------------------------------------------------#
#                           Cleaning R Environment                             #
#------------------------------------------------------------------------------#
rm(list = ls())  
gc()             

#------------------------------------------------------------------------------#
#                                Libraries                                     #
#------------------------------------------------------------------------------#

library(dplyr)
library(survey)
library(srvyr)
library(data.table)
library(magrittr)
library(haven)
library(stringr)
library(tidyr)
library(knitr)
library(kableExtra)
library(broom)
library(ggplot2)
library(scales)
library(forcats)

#------------------------------------------------------------------------------#
#                           Loading Datasets                                   #
#------------------------------------------------------------------------------#


# Load household expenditure dataset (from World Bank survey)
data_sec_expenditure <- read_sav("data/data_ESS4/sect7b_hh_w4_v2.sav") %>% 
  mutate(
    expenditure = ifelse(s7q03 == 2, 0, s7q04), # Assign zero to non-expenditures
    item = as_factor(item_cd_12months)         # Convert item codes to factors
  )

# Convert data to wide format:
# - Items become column names
# - Missing values filled with zero
# - Calculate total expenditure per household
data_expenditure <- data_sec_expenditure %>% 
  select(household_id, item, expenditure) %>% 
  pivot_wider(names_from = item, values_from = expenditure, values_fill = list(expenditure = 0)) %>% 
  mutate(total_expenditure = rowSums(select(., where(is.numeric)), na.rm = TRUE))


# Load household income dataset
data_income <- read_sav("data/data_ESS4/sect13_hh_w4_v2.sav")

# Process income data:
# - Identify if the household received income from a given source (s13q01)
# - Assign zero if no income, otherwise assign reported amount (s13q02)
data_income <- data_income %>% 
  group_by(item = source_cd) %>%
  mutate(
    income = ifelse(s13q01 == 2, 0, s13q02),
    yes_no = ifelse(s13q01 == 2, 0, 1)
  ) 

# Aggregate total income per household, only considering valid income sources
data_income <- data_income %>%  
  filter(s13q01 == 1) %>% 
  group_by(household_id) %>% 
  summarise(total_income = sum(income))

# Merge household expenditure data with income data
data_income <- inner_join(data_expenditure %>% select(household_id, total_expenditure),
                          data_income) 

# Load household characteristics data
data_sec <- read_sav("data/data_ESS4/sect_cover_hh_w4.sav")

# Merge household data with income and expenditure data
data_sec <- data_sec %>% inner_join(data_income)

# Compute per capita expenditure and per capita income
data_sec <- data_sec %>%
  mutate(
    percapita_expenditure = total_expenditure / saq09, 
    percapita_income = total_income / saq09
  )

#------------------------------------------------------------------------------#
#                 Scatter Plots of Expenditure vs Income                       #
#------------------------------------------------------------------------------#

# Scatter plot: Per capita expenditure vs. per capita income (raw values)
ggplot(data_sec, aes(y = percapita_expenditure, x = percapita_income, weight = pw_w4)) +
  geom_point(aes(size = pw_w4), alpha = 0.1) +  # Adjust transparency for readability
  labs(
    title = "Relationship Between Expenditure and Income",
    subtitle = "Point size represents survey weights",
    y = "Per Capita Expenditure",
    x = "Per Capita Income",
    size = "Survey Weights"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center title
    plot.subtitle = element_text(hjust = 0.5)  # Center subtitle
  ) +
  xlim(0, 50000) +  # Limit x-axis to avoid extreme values
  ylim(0, 20000)  # Limit y-axis for better visualization

# Scatter plot: Log-transformed per capita expenditure vs. income
ggplot(data_sec, aes(y = log(percapita_expenditure + 70), x = log(percapita_income + 70), weight = pw_w4)) +
  geom_point(aes(size = pw_w4), alpha = 0.1) +
  labs(
    title = "Relationship Between Expenditure and Income (Log Scale)",
    subtitle = "Point size represents survey weights",
    y = "Log Per Capita Expenditure",
    x = "Log Per Capita Income",
    size = "Survey Weights"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center title
    plot.subtitle = element_text(hjust = 0.5)  # Center subtitle
  )

#------------------------------------------------------------------------------#
#          Scatter Plots by Urban vs. Rural Classification                     #
#------------------------------------------------------------------------------#

# Scatter plot: Raw values by urban/rural classification
ggplot(data_sec %>% mutate(saq14 = as_factor(saq14)), 
       aes(y = percapita_expenditure, x = percapita_income, weight = pw_w4)) +
  geom_point(aes(size = pw_w4), alpha = 0.1) +
  labs(
    title = "Relationship Between Expenditure and Income",
    subtitle = "Point size represents survey weights",
    y = "Per Capita Expenditure",
    x = "Per Capita Income",
    size = "Survey Weights"
  ) + 
  facet_grid(. ~ saq14) +  # Separate plots by urban/rural classification
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center title
    plot.subtitle = element_text(hjust = 0.5)  # Center subtitle
  ) +  
  xlim(0, 50000) +  # Set axis limits
  ylim(0, 20000)

# Scatter plot: Log-transformed values by urban/rural classification
ggplot(data_sec %>% mutate(saq14 = as_factor(saq14)), 
       aes(y = log(percapita_expenditure + 70), x = log(percapita_income + 70), weight = pw_w4)) +
  geom_point(aes(size = pw_w4), alpha = 0.1) +
  labs(
    title = "Relationship Between Expenditure and Income (Log Scale)",
    subtitle = "Point size represents survey weights",
    y = "Log Per Capita Expenditure",
    x = "Log Per Capita Income",
    size = "Survey Weights"
  ) + 
  facet_grid(. ~ saq14) +  # Separate plots by urban/rural classification
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center title
    plot.subtitle = element_text(hjust = 0.5)  # Center subtitle
  )
