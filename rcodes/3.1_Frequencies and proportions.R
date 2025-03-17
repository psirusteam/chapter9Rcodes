################################################################################
# 3.1 Frequencies and Proportions
################################################################################
# Author: Andrés Gutiérrez & Stalyn Guerrero
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
HH_data <- read_sav("data/data_ESS4/sect7b_hh_w4_v2.sav")

# Processing expenditure and response indicators
HH_data <- HH_data %>%
  group_by(item = item_cd_12months) %>%
  mutate(
    expenditure = ifelse(s7q03 == 2, 0, s7q04),
    yes_no = ifelse(s7q03 == 2, 0, 1)
  )

# Defining the survey design
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

# Makes it an error to have a stratum with a single, non-certainty PSU
options(survey.lonely.psu = "fail") 
summary(ESS4_design)

#------------------------------------------------------------------------------#
# TABLE 7.3
# Spending on Nonfood Items and Services
#
# ESS4 Sampled EAs and Households by Region and by Urban and Rural  
# Ethiopia Socioeconomic Survey (ESS) 2018/19
# SURVEY REPORT
# Central Statistics Agency of Ethiopia | World Bank
#------------------------------------------------------------------------------#

tab_01 <- ESS4_design %>%
  group_by(item = item_cd_12months) %>%
  summarise(
  N_hat = survey_total(yes_no, na.rm = TRUE, vartype = c("se", "cv")),
  # Absolute size
  P_hat = survey_mean(yes_no, na.rm = TRUE, vartype = c("se", "cv", "ci"))
  # Proportions
  )

tab_01 %>%
  transmute(
    item = as_factor(item),
    N_hat = round(N_hat),
    N_hat_cv = round(N_hat_cv * 100, 2),
    P_hat = round(P_hat * 100, 2),
    P_hat_cv = round(P_hat_cv * 100, 2),
    P_hat_se = round(P_hat_se * 100, 2),
    P_hat_low = round(P_hat_low * 100, 2),
    P_hat_upp = round(P_hat_upp * 100, 2)
  ) %>% data.frame()

