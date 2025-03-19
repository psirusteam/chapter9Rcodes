################################################################################
# 7.2 Histograms 
################################################################################
# Authors: Andrés Gutiérrez & Stalyn Guerrero
# Economic Comission for Latin America and the Caribbean
# Statistics Division
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

# Ensure select function from dplyr is used explicitly
select <- dplyr::select

#------------------------------------------------------------------------------#
#                           Loading Datasets                                   #
#------------------------------------------------------------------------------#

# Load household expenditure dataset (from World Bank survey)
HH_data <- read_sav("data/data_ESS4/sect7b_hh_w4_v2.sav") %>%
  mutate(expenditure = ifelse(s7q03 == 2, 0, s7q04),
         # Assign zero to non-expenditures
         item = as_factor(item_cd_12months)         
         # Convert item codes to factors
  )

# Convert data to wide format:
# - Items become column names
# - Missing values filled with zero
# - Calculate total expenditure per household
EXP_data <- HH_data %>%
  select(household_id, item, expenditure) %>%
  pivot_wider(
    names_from = item,
    values_from = expenditure,
    values_fill = list(expenditure = 0)
  ) %>%
  mutate(total_expenditure = rowSums(select(., where(is.numeric)), na.rm = TRUE))


# Load additional household data and merge with expenditure dataset
EXP2_data <- read_sav("data/data_ESS4/sect_cover_hh_w4.sav") %>% 
  inner_join(EXP_data) %>%
  # Compute per capita expenditure
  mutate(percapita_expenditure = total_expenditure / saq09)

#------------------------------------------------------------------------------#
#                      Defining Survey Design for Analysis                     #
#------------------------------------------------------------------------------#

# Define survey design:
# - Primary Sampling Unit (PSU) identified by `ea_id`
# - Stratification based on region (`saq01`) and urban/rural status (`saq14`)
# - Sampling weights (`pw_w4`) ensure representativity
ESS4_design <- EXP2_data %>% 
  mutate(strata = paste0(saq01, "_", saq14)) %>% 
  as_survey_design(
    ids = ea_id,  
    strata = strata,  
    weights = pw_w4,  
    nest = TRUE  
  )

options(survey.lonely.psu = "fail") 
summary(ESS4_design)

#------------------------------------------------------------------------------#
#                      Histogram using Survey-Weighted Data                    #
#------------------------------------------------------------------------------#

# Generate a histogram for per capita expenditure using survey weights
svyhist(
  ~ percapita_expenditure,
  ESS4_design,
  main = "Estimated Distribution of Percapita Expenditure",
  col = "grey80",
  breaks = 100, # Number of bins
  xlab = "Expenditure",
  probability = FALSE, # Normalize to probability density
  xlim = c(0, 10000)  # Limit x-axis range
)

#------------------------------------------------------------------------------#
#                      Preparing Data for ggplot2 Visualization                #
#------------------------------------------------------------------------------#

# Create a dataset for visualization:
# - Combine total household expenditures with item-wise expenditures
HH_data2 <- bind_rows(
  EXP2_data %>%
    transmute(
      household_id,
      saq14 = as_factor(saq14),
      expenditure = total_expenditure,
      pw_w4,
      item = as_factor("Total")
    ),
  HH_data %>%
    transmute(
      household_id,
      saq14 = as_factor(saq14),
      expenditure,
      pw_w4,
      item = item
    )
) %>%
  mutate(saq14 = case_when(
    saq14 == "1. RURAL" ~ "RURAL",
    saq14 == "2. URBAN" ~ "URBAN",
    TRUE ~ saq14
  ))

#------------------------------------------------------------------------------#
#                      Histogram for Expenditure Distribution                  #
#------------------------------------------------------------------------------#

ggplot(HH_data2, aes(x = expenditure, weight = pw_w4)) +
  geom_histogram(fill = "grey80", 
                 color = "black", 
                 bins = 20) +
  facet_wrap(. ~ item, nrow = 3, ncol = 4) +  # Arrange plots in a 3x4 grid
  scale_x_continuous(limits = c(0, 10000)) +  # Restrict x-axis range
  scale_y_continuous(limits = c(0, 5000000), labels = label_number(accuracy = 1)) +
  labs(
    y = "Estimated Frequency",
    x = "Expenditure",
    title = "Estimated Distribution of Percapita Expenditure by Item"
  ) +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5))  # Center title

#------------------------------------------------------------------------------#
#          Histogram Comparing Urban vs. Rural Expenditure Distribution        #
#------------------------------------------------------------------------------#

ggplot(HH_data2, aes(x = expenditure, weight = pw_w4,
                     fill = saq14)) +
  geom_histogram(color = "black", 
                 bins = 20, 
                 position = "identity",
                 alpha = 0.5) +
  facet_wrap(. ~ item, nrow = 3, ncol = 4) +
  scale_x_continuous(limits = c(0, 10000)) +
  scale_y_continuous(limits = c(0, 5000000), labels = label_number(accuracy = 1)) +
  scale_fill_manual(values = c("URBAN" = "#E6B9AA", "RURAL" = "blue")) +  # Light pastel colors
  labs(
    y = "Estimated Frequency",
    x = "Expenditure",
    title = "Estimated Distribution of Percapita Expenditure by Item",
    fill = "Zone"
  ) +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5))  # Center title
