################################################################################
# 7.3 Scatter Plots - Relationship Between Expenditure and Income
################################################################################
# Authors: Andrés Gutiérrez & Stalyn Guerrero
# Economic Comission for Latin America and the Caribbean
# Statistics Division
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

# Ensure select function from dplyr is used explicitly
select <- dplyr::select


#------------------------------------------------------------------------------#
#                           Loading Datasets                                   #
#------------------------------------------------------------------------------#


# Load household expenditure dataset (from World Bank survey)
EXP_data <- read_sav("data/data_ESS4/sect7b_hh_w4_v2.sav") %>% 
  mutate(
    expenditure = ifelse(s7q03 == 2, 0, s7q04), # Assign zero to non-expenditures
    item = as_factor(item_cd_12months)         # Convert item codes to factors
  )

# Convert data to wide format:
# - Items become column names
# - Missing values filled with zero
# - Calculate total expenditure per household
EXP2_data <- EXP_data %>% 
  select(household_id, item, expenditure) %>% 
  pivot_wider(names_from = item, values_from = expenditure, values_fill = list(expenditure = 0)) %>% 
  mutate(total_expenditure = rowSums(select(., where(is.numeric)), na.rm = TRUE))


# Load household income dataset
INC_data <- read_sav("data/data_ESS4/sect13_hh_w4_v2.sav")

# Process income data:
# - Identify if the household received income from a given source (s13q01)
# - Assign zero if no income, otherwise assign reported amount (s13q02)
INC_data <- INC_data %>% 
  group_by(item = source_cd) %>%
  mutate(
    income = ifelse(s13q01 == 2, 0, s13q02),
    yes_no = ifelse(s13q01 == 2, 0, 1)
  ) 

# Aggregate total income per household, only considering valid income sources
INC_data <- INC_data %>%  
  filter(s13q01 == 1) %>% 
  group_by(household_id) %>% 
  summarise(total_income = sum(income))

# Merge household expenditure data with income data
INC_data <- inner_join(EXP2_data %>% select(household_id, total_expenditure),
                          INC_data) 

# Load household characteristics data
HH_data <- read_sav("data/data_ESS4/sect_cover_hh_w4.sav")

# Merge household data with income and expenditure data
HH_data <- HH_data %>% inner_join(INC_data)

# Compute per capita expenditure and per capita income
HH_data <- HH_data %>%
  mutate(
    percapita_expenditure = total_expenditure / saq09,
    log_expenditure = log(percapita_expenditure),
    percapita_income = total_income / saq09,
    log_income = log(percapita_income)
    
  )

#------------------------------------------------------------------------------#
#                 Scatter Plots of Expenditure vs Income                       #
#------------------------------------------------------------------------------#

# Scatter plot: Log-transformed per capita expenditure vs. income
ggplot(HH_data, aes(y = log_expenditure, 
                     x = log_income, weight = pw_w4)) +
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
  ) +
  geom_smooth(method = "lm", 
              color = scales::alpha("red", 0.1), 
              fill= "blue", 
              alpha = 0.05)
