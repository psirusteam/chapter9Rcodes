################################################################################
# 2.3 Using Software to Generate Valid Inferences
################################################################################
# Authors: Andrés Gutiérrez & Stalyn Guerrero
# 
# Data Source:
# https://microdata.worldbank.org/index.php/catalog/3823/data-dictionary 
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

library(tidyverse)
library(survey)
library(srvyr)
library(data.table)
library(magrittr)
library(haven)
library(stringr)
library(tidyr)
library(knitr)
library(kableExtra)

# Ensure select function from dplyr is used explicitly
select <- dplyr::select

#------------------------------------------------------------------------------#
#                        Setting the RAM Memory Limit                          #
#------------------------------------------------------------------------------#

memory.limit(250000000)

#------------------------------------------------------------------------------#
#                           Loading Datasets                                   #
#------------------------------------------------------------------------------#

# Household identification; location; household size and field staff identification 
HH_data <- read_sav("data/data_ESS4/sect_cover_hh_w4.sav") %>% 
  select(
    household_id, # Unique Household Identifier 
    ea_id,        # Unique Enumeration Area Identifier
    saq14,        # Area = Rural/Urban
    saq02,        # Zone Code
    saq01,        # Region Code 
    pw_w4         # Final adjusted sampling weight
  )


#------------------------------------------------------------------------------#
# Ethiopia Socioeconomic Survey (ESS) 2018/19
# ESS4 Sampled EAs and Households by Region and by Urban and Rural  
# SURVEY REPORT - TABLE 1.1A
# Central Statistics Agency of Ethiopia | World Bank
#------------------------------------------------------------------------------#

# Count of Enumeration Areas (EA) by saq01 and saq14 categories

ea_counts_by_region <- HH_data %>% 
  distinct(ea_id, saq14, saq01) %>% 
  mutate(saq14 = as_factor(saq14), saq01 = as_factor(saq01)) %>% 
  group_by(saq01, saq14) %>% 
  tally() %>% 
  pivot_wider(names_from = saq14, values_from = n, values_fill = list(n = 0)) %>% 
  mutate(ea_counts_by_region = RURAL + URBAN) %>% 
  rename_with(~ paste0(., "_ea_id"), c("RURAL", "URBAN"))

# Count of Households (HH) by region and area (urban/rural) categories

hh_counts_by_region <- HH_data %>% 
  distinct(household_id, saq01, saq14) %>% 
  mutate(saq14 = as_factor(saq14), saq01 = as_factor(saq01)) %>% 
  group_by(saq01, saq14) %>% 
  tally() %>% 
  pivot_wider(names_from = saq14, values_from = n, values_fill = list(n = 0)) %>% 
  mutate(TOTAL_HH = RURAL + URBAN) %>% 
  rename_with(~ paste0(., "_HH"), c("RURAL", "URBAN"))

# Merging both datasets on saq01
temp <- inner_join(hh_counts_by_region, ea_counts_by_region)

# Summarizing the results including an additional row for "ETHIOPIA"
survey_counts <- bind_rows(
  temp,
  temp %>% data.frame() %>% summarise_if(is.numeric, sum) %>%
    mutate(saq01 = "ETHIOPIA")
) %>% 
  select(
    Region = saq01, 
    RURAL_HH, 
    RURAL_AE = RURAL_ea_id,
    URBAN_HH, 
    URBAN_AE = URBAN_ea_id,
    TOTAL_HH,
    TOTAL_AE = ea_counts_by_region
  ) %>% 
  data.frame()

# Display final result
data.frame(survey_counts)
#------------------------------------------------------------------------------#
# Defining the survey design
#------------------------------------------------------------------------------#

ESS4_design <- HH_data %>% 
  mutate(strata = paste0(saq01, "_", saq14)) %>% 
  as_survey_design(
    ids = ea_id,  # Primary sampling unit identifier (EA)
    strata = strata, # Stratification by region (saq01) and urban/rural zone (saq14)
    weights = pw_w4,  # Final adjusted weight
    nest = TRUE
  )

# Makes it an error to have a stratum with a single, non-certainty PSU
options(survey.lonely.psu = "fail") 
summary(ESS4_design)
