################################################################################
# 7.2 Histograms 
################################################################################
# Author: Andrés Gutiérrez, Stalyn Guerrero
# 
# Description:
# This section generates histograms to visualize the distribution of per capita 
# household expenditures on various non-food items and services. The expenditure 
# data is weighted using survey weights to ensure representativity.
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


# Load additional household data and merge with expenditure dataset
data_sec <- read_sav("data/data_ESS4/sect_cover_hh_w4.sav") %>% 
  inner_join(data_expenditure)

# Compute per capita expenditure (total household expenditure divided by household size)
data_sec <- data_sec %>%
  mutate(percapita_expenditure = total_expenditure / saq09)

#------------------------------------------------------------------------------#
#                      Defining Survey Design for Analysis                     #
#------------------------------------------------------------------------------#

# Define survey design:
# - Primary Sampling Unit (PSU) identified by `ea_id`
# - Stratification based on region (`saq01`) and urban/rural status (`saq14`)
# - Sampling weights (`pw_w4`) ensure representativity
design_sampling <- data_sec %>% 
  mutate(strata = paste0(saq01, "_", saq14)) %>% 
  as_survey_design(
    ids = ea_id,  
    strata = strata,  
    weights = pw_w4,  
    nest = TRUE  
  )

#------------------------------------------------------------------------------#
#                      Histogram using Survey-Weighted Data                    #
#------------------------------------------------------------------------------#

# Generate a histogram for per capita expenditure using survey weights
svyhist(
  ~ percapita_expenditure,
  design_sampling,
  main = "Population Expenditure",
  col = "grey80",
  breaks = 100, # Number of bins
  xlab = "Expenditure",
  probability = TRUE, # Normalize to probability density
  xlim = c(0, 10000)  # Limit x-axis range
)

#------------------------------------------------------------------------------#
#                      Preparing Data for ggplot2 Visualization                #
#------------------------------------------------------------------------------#

# Create a dataset for visualization:
# - Combine total household expenditures with item-wise expenditures
data_sec_expenditure2 <- bind_rows(
  data_sec %>%
    transmute(household_id, saq14 = as_factor(saq14),
              expenditure = total_expenditure, pw_w4, item = as_factor("Total")),
  data_sec_expenditure %>%
    transmute(household_id, saq14 = as_factor(saq14), expenditure, pw_w4, 
              item = item)
) %>% 
  mutate(saq14 = case_when(
    saq14 == "1. RURAL" ~ "RURAL", 
    saq14 == "2. URBAN" ~ "URBAN",
    TRUE ~ saq14
  ))

#------------------------------------------------------------------------------#
#                      Histogram for Expenditure Distribution                  #
#------------------------------------------------------------------------------#

ggplot(data_sec_expenditure2, aes(x = expenditure, weight = pw_w4)) +
  geom_histogram(aes(y = after_stat(density)), fill = "grey80", color = "black", bins = 50) +
  facet_wrap(. ~ item, nrow = 3, ncol = 4) +  # Arrange plots in a 3x4 grid
  scale_x_continuous(limits = c(0, 15000)) +  # Restrict x-axis range
  scale_y_continuous(limits = c(0, 0.0005), labels = label_number(accuracy = 0.00005)) +
  labs(
    y = "Probability Density",
    x = "Expenditure",
    title = "Population of Expenditure by Item"
  ) +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5))  # Center title

#------------------------------------------------------------------------------#
#          Histogram Comparing Urban vs. Rural Expenditure Distribution        #
#------------------------------------------------------------------------------#

ggplot(data_sec_expenditure2, aes(x = expenditure, weight = pw_w4,
                                  fill = saq14)) +
  geom_histogram(aes(y = after_stat(density)), color = "black", bins = 50, position = "identity") +
  facet_wrap(. ~ item, nrow = 3, ncol = 4) +
  scale_x_continuous(limits = c(0, 15000)) +
  scale_y_continuous(limits = c(0, 0.0005), labels = label_number(accuracy = 0.00005)) +
  scale_fill_manual(values = c("RURAL" = "#E6B9AA", "URBAN" = "#A9CCE9")) +  # Light pastel colors
  labs(
    y = "Probability Density",
    x = "Expenditure",
    title = "Population of Expenditure by Item",
    fill = "Zone"
  ) +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5))  # Center title
