################################################################################
# 3.2 Totals, means and ratios
################################################################################
# Author: Andrés Gutiérrez, Stalyn Guerrero
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

data_sec <- read_sav("data/data_ESS4/sect7b_hh_w4_v2.sav")

#------------------------------------------------------------------------------#
#                Calculating Ratio of Expenditures                            #
#------------------------------------------------------------------------------#

# Processing data 
data_sec <- data_sec %>%
  mutate(expenditure = ifelse(s7q03 == 2, 0, s7q04)) %>% 
  group_by(household_id) %>%
  mutate(total_expenditure = sum(expenditure, na.rm = TRUE))

# Defining survey design

options(survey.lonely.psu = "fail") 

design_sampling <- data_sec %>% 
  mutate(strata = paste0(saq01, "_", saq14)) %>% 
  as_survey_design(
    ids = ea_id,  # Primary sampling unit identifier (EA)
    strata = strata, # Stratification by region (saq01) and urban/rural zone (saq14)
    weights = pw_w4,  # Final adjusted weight
    nest = TRUE
  )

summary(design_sampling)
#------------------------------------------------------------------------------#
# Estimation of totals and means
#------------------------------------------------------------------------------#

tab_02 <- design_sampling %>%   group_by(item = item_cd_12months) %>% 
  summarise(
  T_hat = survey_total(expenditure, na.rm = TRUE, vartype = c("se", "cv")),
  M_hat = survey_mean(expenditure, na.rm = TRUE, vartype = c("se", "cv"))
)

tab_02 %>%
  transmute(
    item = as_factor(item),
    T_hat = round(T_hat),
    T_hat_cv = round(T_hat_cv * 100, 2),
    M_hat = round(M_hat),
    M_hat_cv = round(M_hat_cv * 100, 2)
  ) %>%
  kable(
    digits = 2
  )


#------------------------------------------------------------------------------#
# Estimation of Ratio                                            #
#------------------------------------------------------------------------------#

tab_03_R1_total <- design_sampling %>%
  group_by(item = item_cd_12months) %>% 
  summarise(R_hat = survey_ratio(
    expenditure,
    total_expenditure,
    na.rm = TRUE,
    vartype = c("se", "cv")
  )) %>% mutate(Region = "Ethiopia")


tab_03_R1_region <-
  design_sampling %>% 
  group_by(item = item_cd_12months, Region = as_factor(saq01)) %>%
  summarise(R_hat = survey_ratio(
    expenditure,
    total_expenditure,
    na.rm = TRUE,
    vartype = c("se", "cv")
  ))

# Combining results
tab_03_R1 <- bind_rows(tab_03_R1_total, tab_03_R1_region)


# Formatting output
temp <- tab_03_R1 %>%
  transmute(
    Region, 
    item = as_factor(item),
    R_hat = round(R_hat*100, 2),
    R_hat = paste0(R_hat, " (", round(R_hat_cv*100, 2), ")") 
  ) %>%
  pivot_wider(names_from = Region, values_from = R_hat, values_fill = list(n = 0))

# Displaying results
temp %>%   kable(digits = 2)


#------------------------------------------------------------------------------#
#                Calculating Ratio of Educational Expenditure                  #
#------------------------------------------------------------------------------#

data_sec <- read_sav("data/data_ESS4/cons_agg_w4.sav")

# Defining survey design
design_sampling <- data_sec %>% 
  mutate(strata = paste0(saq01, "_", saq14)) %>% 
  as_survey_design(
    ids = ea_id,  # Primary sampling unit identifier (EA)
    strata = strata, # Stratification by region (saq01) and urban/rural zone (saq14)
    weights = pw_w4,  # Final adjusted weight
    nest = TRUE
  )

#------------------------------------------------------------------------------#
# Estimation of Ratio                                            #
#------------------------------------------------------------------------------#

tab_03_R2_total <- design_sampling %>% 
  summarise(R_hat = survey_ratio(
    educ_cons_ann,
    total_cons_ann,
    na.rm = TRUE,
    vartype = c("se", "cv")
  )) %>% mutate(Region = "Ethiopia")


tab_03_R2_region <-
  design_sampling %>% group_by(Region = as_factor(saq01)) %>% 
  summarise(R_hat = survey_ratio(
    educ_cons_ann,
    total_cons_ann,
    na.rm = TRUE,
    vartype = c("se", "cv")
  ))


# Combining results
tab_03_R2 <- bind_rows(tab_03_R2_region, tab_03_R2_total)

# Formatting and displaying results
tab_03_R2 %>%
  transmute(Region,
            R_hat = round(R_hat * 100, 2),
            R_hat_cv = round(R_hat_cv * 100, 2))  %>%
  kable(digits = 2)

