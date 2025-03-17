################################################################################
# 4.2 Cross-tabulations
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
library(convey)

# Ensure select function from dplyr is used explicitly
select <- dplyr::select

#------------------------------------------------------------------------------#
#                        Setting the RAM Memory Limit                          #
#------------------------------------------------------------------------------#

memory.limit(250000000)

#------------------------------------------------------------------------------#
#                           Loading Datasets                                   #
#------------------------------------------------------------------------------#

options(survey.lonely.psu = "fail") 
data_sec <- read_sav("data/data_ESS4/sect1_hh_w4.sav")

#------------------------------------------------------------------------------#
#                         Defining Survey Design                              #
#------------------------------------------------------------------------------#

design_sampling <- data_sec %>%
  mutate(strata = paste0(saq01, "_", saq14)) %>%
  as_survey_design(
    ids = ea_id, # Primary sampling unit identifier (EA)
    strata = strata, # Stratification by region (saq01) and urban/rural zone (saq14)
    weights = pw_w4,  # Final adjusted weight
    nest = TRUE
  )

#------------------------------------------------------------------------------#
#                   Processing Variables for Analysis                         #
#------------------------------------------------------------------------------#

design_sampling <- design_sampling %>%
  mutate(
    sexo = as_factor(s1q02),
    religion = as_factor(s1q08),
    religion = case_when(
      religion %in% c("6. PEGAN", "5. TRADITIONAL", "8. OTHER (SPECIFY)", 
                      "7. WAKEFETA") |
        is.na(religion) ~ "OTHER",
      TRUE ~ religion
    )
  ) %>%
  filter(s1q03a >= 10)

#------------------------------------------------------------------------------#
#               Estimating Proportions and Totals by Religion                 #
#------------------------------------------------------------------------------#

# Estimates by region
tab_01_region <- design_sampling %>%
  group_by(Region = as_factor(saq01), religion) %>%
  summarise(
    P_hat = survey_mean(vartype = c("se", "cv")),
    T_hat = survey_total(vartype = c("se", "cv"))
  )

# National estimates
tab_01_tot <- design_sampling %>%
  group_by(Region = "Ethiopia", religion) %>%
  summarise(
    P_hat = survey_mean(vartype = c("se", "cv")), 
    T_hat = survey_total(vartype = c("se", "cv"))
  )

# Estimates by urban/rural zones
tab_01_zone <- design_sampling %>%
  group_by(Region = as_factor(saq14), religion) %>%
  summarise(
    P_hat = survey_mean(vartype = c("se", "cv")),
    T_hat = survey_total(vartype = c("se", "cv"))
  )

#------------------------------------------------------------------------------#
#                  Formatting and Displaying Cross-tabulation Tables          #
#------------------------------------------------------------------------------#

# Proportion table
temp1 <- bind_rows(tab_01_region, tab_01_zone, tab_01_tot) %>% 
  select(P_hat, religion) %>%
  mutate(P_hat = round(P_hat * 100, 1)) %>% 
  pivot_wider(
    names_from = religion,
    values_from = P_hat,
    values_fill = list(P_hat = 0)
  ) 

temp1 %>% 
  select(
    Region, Orthodox = "1. ORTHODOX",
    Catholic = "2. CATHOLIC",
    Protestant = "3. PROTESTANT",
    Muslim = "4. MUSLEM", 
    Other = OTHER
  ) %>% 
  kable(digits = 2)

# Total estimates table
temp2 <- bind_rows(tab_01_region, tab_01_zone, tab_01_tot) %>% 
  select(T_hat, religion) %>%
  mutate(T_hat = round(T_hat)) %>% 
  pivot_wider(
    names_from = religion,
    values_from = T_hat,
    values_fill = list(T_hat = 0)
  ) 

temp2 %>% 
  select(
    Region, Orthodox = "1. ORTHODOX",
    Catholic = "2. CATHOLIC",
    Protestant = "3. PROTESTANT",
    Muslim = "4. MUSLEM", 
    Other = OTHER
  ) %>% 
  kable(digits = 2)

