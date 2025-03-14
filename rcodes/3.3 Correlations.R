################################################################################
# 3.3 Correlations
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
#                           Loading Datasets                                   #
#------------------------------------------------------------------------------#

dat_pers <- read_sav("data/data_ESS4/cons_agg_w4.sav") %>% 
  mutate(strata = paste0(saq01, "_", saq14))

#------------------------------------------------------------------------------#
#                       Defining Survey Design                                #
#------------------------------------------------------------------------------#
options(survey.lonely.psu = "fail") 

design_sampling <- dat_pers %>% 
  as_survey_design(
    ids = ea_id,  # Primary sampling unit identifier (EA)
    strata = strata, # Stratification by region (saq01) and urban/rural zone (saq14)
    weights = pw_w4,  # Final adjusted weight
    nest = TRUE
  )

#------------------------------------------------------------------------------#
#                   Computing Correlations                                    #
#------------------------------------------------------------------------------#

# Overall correlation
design_sampling %>%
  summarise(
    corr1 = survey_corr(educ_cons_ann, utilities_cons_ann), 
    corr2 = survey_corr(educ_cons_ann, fafh_cons_ann)
  ) %>% 
  kable(digits = 2)

# Correlation grouped by urban/rural area
design_sampling %>% 
  group_by(area = as_factor(saq14)) %>%
  summarise(
    corr1 = survey_corr(educ_cons_ann, utilities_cons_ann), 
    corr2 = survey_corr(educ_cons_ann, fafh_cons_ann)
  ) %>% 
  kable(digits = 2)

# Correlation grouped by region
design_sampling %>% 
  group_by(Region = as_factor(saq01)) %>%
  summarise(
    corr1 = survey_corr(educ_cons_ann, utilities_cons_ann), 
    corr2 = survey_corr(educ_cons_ann, fafh_cons_ann)
  ) %>% 
  kable(digits = 2)


