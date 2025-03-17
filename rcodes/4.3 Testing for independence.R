################################################################################
# 4.3 Testing for Independence
################################################################################
# Author: Andrés Gutiérrez & Stalyn Guerrero
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

data_sec <- read_sav("data/data_ESS4/sect1_hh_w4.sav")

#------------------------------------------------------------------------------#
#                    Processing Education Variables                           #
#------------------------------------------------------------------------------#

data_sec <- data_sec %>% 
  mutate(
    education_mother = case_when(
      s1q20 %in% c(98, 99, 93) ~ "01 No education",  
      s1q20 %in% c(96, 94) ~ "02 Primary",  
      s1q20 >= 0 & s1q20 <= 8 ~ "02 Primary",  
      s1q20 %in% c(9:13, 21:26, 95) ~ "03 Secondary",  
      is.na(s1q20) ~ NA_character_, 
      TRUE ~ "04 Above secondary"
    ),
    education_father = case_when(
      s1q16 %in% c(98, 99, 93) ~ "01 No education",  
      s1q16 %in% c(96, 94) ~ "02 Primary",  
      s1q16 >= 0 & s1q16 <= 8 ~ "02 Primary",  
      s1q16 %in% c(9:13, 21:26, 95) ~ "03 Secondary",  
      is.na(s1q16) ~ NA_character_, 
      TRUE ~ "04 Above secondary"
    ),
    zone = as_factor(saq14)
  )

#------------------------------------------------------------------------------#
#                          Defining Survey Design                              #
#------------------------------------------------------------------------------#

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
# TABLE 2.4 - Education and Occupation of Biological Parents                   #
# Source: Central Statistics Agency of Ethiopia & World Bank (2018/19)         #
# Ethiopia Socioeconomic Survey (ESS) Survey Report                            #
#------------------------------------------------------------------------------#

## National estimates for father
tab_02_tot_father <- design_sampling %>% 
  filter(s1q03a < 18, !is.na(education_father)) %>%
  group_by(Education = education_father) %>%
  summarise(P_father = survey_mean(vartype = c("se", "cv"))) %>%
  mutate(father = round(P_father * 100, 1))

## National estimates for mother
tab_02_tot_mother <- design_sampling %>% 
  filter(s1q03a < 18, !is.na(education_mother)) %>%
  group_by(Education = education_mother) %>%
  summarise(P_mother = survey_mean(vartype = c("se", "cv"))) %>%
  mutate(mother = round(P_mother * 100, 1))

## Estimates by zone for father
tab_02_zone_father <- design_sampling %>% 
  filter(s1q03a < 18, !is.na(education_father)) %>%
  group_by(zone, Education = education_father) %>%
  summarise(P_father = survey_mean(vartype = c("se", "cv"))) 

## Estimates by zone for mother
tab_02_zone_mother <- design_sampling %>%
  filter(s1q03a < 18, !is.na(education_mother)) %>%
  group_by(zone, Education = education_mother) %>%
  summarise(P_mother = survey_mean(vartype = c("se", "cv")))

## Merging tables
tab_02 <- inner_join(tab_02_tot_mother, tab_02_tot_father) %>%
  inner_join(inner_join(tab_02_zone_father, tab_02_zone_mother)) %>%
  select(Education, father, mother, matches("RURAL"), matches("URBAN"))

tab_02 %>% kable(digits = 2)

#------------------------------------------------------------------------------#
#                   Testing for Independence                                   #
#------------------------------------------------------------------------------#

## Cross-tabulations
svytable(~ education_mother + zone, design_sampling, Ntotal = 1) %>% 
  addmargins() %>% 
  kable()

svytable(~ education_father + zone, design_sampling, Ntotal = 1) %>% 
  addmargins() %>% 
  kable()

## Chi-square tests
svychisq(~ education_mother + zone, design_sampling)
svychisq(~ education_father + zone, design_sampling)



