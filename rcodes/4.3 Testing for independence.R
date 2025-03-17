################################################################################
# 4.3 Testing for Independence
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

HH_data <- read_sav("data/data_ESS4/sect1_hh_w4.sav")

#------------------------------------------------------------------------------#
#                    Processing Education Variables                            #
#------------------------------------------------------------------------------#

HH_data <- HH_data %>%
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
#                          Defining the Survey Design                          #
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

options(survey.lonely.psu = "fail")
summary(ESS4_design)

#------------------------------------------------------------------------------#
# TABLE 2.4 - Education and Occupation of Biological Parents                   #
# Source: Central Statistics Agency of Ethiopia & World Bank (2018/19)         #
# Ethiopia Socioeconomic Survey (ESS) Survey Report                            #
#------------------------------------------------------------------------------#

## National estimates for father`s education
tab_02_tot_father <- ESS4_design %>%
  filter(s1q03a < 18,!is.na(education_father)) %>%
  group_by(Education = education_father) %>%
  summarise(P_father_National = survey_mean(vartype = c("se", "cv")))

## National estimates for mother's education
tab_02_tot_mother <- ESS4_design %>%
  filter(s1q03a < 18,!is.na(education_mother)) %>%
  group_by(Education = education_mother) %>%
  summarise(P_mother_National = survey_mean(vartype = c("se", "cv")))

## Estimates by zone for father
tab_02_zone_father <- ESS4_design %>%
  filter(s1q03a < 18,!is.na(education_father)) %>%
  group_by(zone, Education = education_father) %>%
  summarise(P_father = survey_mean(vartype = c("se", "cv")))

tab_02_zone_father_P <- tab_02_zone_father %>%
  transmute(zone, Education      ,
            P_father) %>%
  pivot_wider(
    names_from = zone ,
    values_from = P_father,
    values_fill = list(P_father = 0),
    names_prefix = "P_father"
  )

## Estimates by zone for mother
tab_02_zone_mother <- ESS4_design %>%
  filter(s1q03a < 18,!is.na(education_mother)) %>%
  group_by(zone, Education = education_mother) %>%
  summarise(P_mother = survey_mean(vartype = c("se", "cv")))

tab_02_zone_mother_P <- tab_02_zone_mother %>%
  transmute(zone, Education ,
            P_mother)  %>%
  pivot_wider(
    names_from = zone ,
    values_from = P_mother,
    values_fill = list(P_mother = 0),
    names_prefix = "P_mother"
  )

## Merging tables
tab_02 <- inner_join(tab_02_tot_mother, tab_02_tot_father) %>%
  inner_join(inner_join(tab_02_zone_father_P, tab_02_zone_mother_P)) %>%
  select(
    Education,
    P_father_National,
    P_mother_National,
    matches("RURAL"),
    matches("URBAN")
  )

tab_02

#------------------------------------------------------------------------------#
#                   Testing for Independence                                   #
#------------------------------------------------------------------------------#


## Cross-tabulations
svytable(~ education_mother + zone, ESS4_design, Ntotal = 1) %>%
  addmargins()

svytable(~ education_father + zone, ESS4_design, Ntotal = 1) %>%
  addmargins()

## Chi-square tests
svychisq(~ education_mother + zone, ESS4_design)
svychisq(~ education_father + zone, ESS4_design)

