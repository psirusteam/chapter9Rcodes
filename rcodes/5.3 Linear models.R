################################################################################
# 5.3 Linear Models
################################################################################
# Authors: Andrés Gutiérrez & Stalyn Guerrero
# Economic Comission for Latin America and the Caribbean
# Statistics Division
#
# Description:
# This script loads and prepares household survey data, including expenditure
# calculations, demographic characteristics, and survey weighting adjustments,
# followed by the estimation of linear models.
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
library(ggplot2)
library(svydiags)
library(broom)

# Ensure the correct use of dplyr's select function
select <- dplyr::select

#------------------------------------------------------------------------------#
#                           Loading Datasets                                   #
#------------------------------------------------------------------------------#

## Household expenditure data
EXP_data <- read_sav("data/data_ESS4/sect7b_hh_w4_v2.sav") %>%
  mutate(expenditure = ifelse(s7q03 == 2, 0, s7q04)) %>%
  group_by(household_id) %>%
  summarise(total_expenditure = sum(expenditure, na.rm = TRUE))

## General household information
IND_data <- read_sav("data/data_ESS4/sect1_hh_w4.sav")

#------------------------------------------------------------------------------#
#                     Processing Demographic Information                       #
#------------------------------------------------------------------------------#

## Age grouping and demographic characteristics
IND_data_age <- IND_data %>%
  transmute(
    household_id,
    individual_id,
    ea_id,
    # Primary sampling unit identifier (EA)
    pw_w4,
    saq01,
    # Region
    Zone = as_factor(saq14),
    # Urban/Rural classification
    strata = paste0(saq01, "_", saq14),
    # Stratification region-zone
    s1q03a,
    # Age in years
    age_group = case_when(
      s1q03a < 18 ~ NA_character_,
      s1q03a < 31 ~ "18-30",
      s1q03a < 46 ~ "31-45",
      s1q03a < 66 ~ "46-65",
      TRUE ~ "66 or more"
    ),
    Sex = as_factor(s1q02),
    Religion = as_factor(s1q08) %>%
      recode(
        "6. PEGAN" = "OTHER",
        "5. TRADITIONAL" = "OTHER",
        "8. OTHER (SPECIFY)" = "OTHER",
        "7. WAKEFETA" = "OTHER"
      ) %>%
      replace_na("OTHER")
  )

#------------------------------------------------------------------------------#
#                      Processing Marital Status                               #
#------------------------------------------------------------------------------#

IND_data1 <- read_sav("data/data_ESS4/sect1_hh_w4.sav")

IND_data1_marital <- IND_data1 %>%
  transmute(household_id,
            individual_id,
            s1q09 = as.character(as_factor(s1q09)) %>%
              replace_na("not answered"))

#------------------------------------------------------------------------------#
#                     Household Expenditure Per Capita                         #
#------------------------------------------------------------------------------#

HH_data <- read_sav("data/data_ESS4/sect_cover_hh_w4.sav") %>%
  inner_join(EXP_data, by = "household_id")

HH_data_expenditure <- HH_data %>%
  transmute(household_id,
            percapita_expenditure = total_expenditure / saq09)

#------------------------------------------------------------------------------#
#                 Merging Datasets for Regression Analysis                     #
#------------------------------------------------------------------------------#

IND_data_regression <- IND_data_age %>%
  inner_join(IND_data1_marital, by = c("household_id", "individual_id")) %>%
  inner_join(HH_data_expenditure, by = "household_id") %>%
  filter(s1q03a >= 18)

# Saving processed dataset
saveRDS(IND_data_regression,
        "data/data_ESS4/IND_data_regression.rds")

#------------------------------------------------------------------------------#
#                          Defining Survey Design                              #
#------------------------------------------------------------------------------#

ESS4_design <- IND_data_regression %>%
  filter(percapita_expenditure > 0) %>%
  as_survey_design(
    ids = ea_id,
    # Primary sampling unit identifier (EA)
    strata = strata,
    # Stratification by region (saq01) and urban/rural zone (Zone)
    weights = pw_w4,
    # Final adjusted weight
    nest = TRUE
  ) %>%
  mutate(log_expenditure = log(percapita_expenditure + 70))

options(survey.lonely.psu = "fail") # Adjust to handle singleton PSUs
summary(ESS4_design)

#------------------------------------------------------------------------------#
#                         Estimating Linear Models                             #
#------------------------------------------------------------------------------#

# Survey-weighted linear model for log expenditure
EXP_model <-
  svyglm(log_expenditure  ~ 1 + Zone * Religion + Zone + Sex + age_group,
         design = ESS4_design)

# Display model coefficients
summary(EXP_model)
EXP_model %>% coef()


