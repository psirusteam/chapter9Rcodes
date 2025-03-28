################################################################################
# 4.4 Tests for Group Comparisons
################################################################################
# Authors: Andrés Gutiérrez & Stalyn Guerrero
# Economic Comission for Latin America and the Caribbean
# Statistics Division
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

EXP_data <- read_sav("data/data_ESS4/sect7b_hh_w4_v2.sav") %>%
  mutate(expenditure = ifelse(s7q03 == 2, 0, s7q04)) %>%
  group_by(household_id) %>%
  summarise(total_expenditure = sum(expenditure, na.rm = TRUE))

HH_data <- read_sav("data/data_ESS4/sect_cover_hh_w4.sav")

#------------------------------------------------------------------------------#
#                      Processing Per Capita Expenditure                       #
#------------------------------------------------------------------------------#

# Merging household data with expenditure data
HH_data <- HH_data %>% 
  inner_join(EXP_data) %>% 
  mutate(percapita_expenditure = total_expenditure / saq09)

#------------------------------------------------------------------------------#
#                          Defining Survey Design                              #
#------------------------------------------------------------------------------#

ESS4_design <- HH_data %>%
  mutate(strata = paste0(saq01, "_", saq14)) %>%
  as_survey_design(
    ids = ea_id,
    # Primary sampling unit identifier (EA)
    strata = strata,
    # Stratification by region (saq01) and urban/rural zone (saq14)
    weights = pw_w4,
    # Final adjusted weight
    nest = TRUE
  )

#------------------------------------------------------------------------------#
#                   Conducting Group Comparisons on expenditure                #
#------------------------------------------------------------------------------#

# Mean estimates by urban/rural classification
ESS4_design %>%
  group_by(saq14 = as_factor(saq14)) %>%
  summarise(M_hat = survey_mean(percapita_expenditure, vartype = c("se", "ci"))) %>%
  data.frame()

# Mean estimates by region and urban/rural classification
ESS4_design %>%
  group_by(saq01 = as_factor(saq01), saq14 = as_factor(saq14)) %>%
  summarise(M_hat = survey_mean(percapita_expenditure, vartype = c("se", "ci"))) %>%
  data.frame()

#------------------------------------------------------------------------------#
#                 Hypothesis Testing for Group Comparisons                     #
#------------------------------------------------------------------------------#

# Overall comparison between urban and rural areas
svyttest(percapita_expenditure ~ saq14,
         design = ESS4_design,
         level = 0.95)

# Comparison within region 1
svyttest(
  percapita_expenditure ~ saq14,
  design = ESS4_design %>% filter(saq01 == 1),
  level = 0.95
)

# Comparison within region 2
svyttest(
  percapita_expenditure ~ saq14,
  design = ESS4_design %>% filter(saq01 == 2),
  level = 0.95
)
