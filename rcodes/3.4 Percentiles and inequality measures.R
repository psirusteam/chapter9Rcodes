################################################################################
# 3.4 Percentiles and inequality measures
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

HH_data <- read_sav("data/data_ESS4/sect13_hh_w4_v2.sav")

#------------------------------------------------------------------------------#
#                Processing Income Data                                        #
#------------------------------------------------------------------------------#

HH_data <- HH_data %>%
  group_by(item = source_cd) %>%
  mutate(income = ifelse(s13q01 == 2, 0, s13q02),
         yes_no = ifelse(s13q01 == 2, 0, 1))

#------------------------------------------------------------------------------#
# Defining Survey Design                                                       #
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
# Estimation of Income Statistics - TABLE 5.3                                  #
# Other Income by Source in the Last 12 Months and Median Income by Source,
# Ethiopia 2018/19
#------------------------------------------------------------------------------#
# ESS4 Sampled EAs and Households by Region and by Urban and Rural
# Ethiopia Socioeconomic Survey (ESS) 2018/19
# SURVEY REPORT
# Central Statistics Agency of Ethiopia | World Bank
#------------------------------------------------------------------------------#

# Estimating the Mean and Median of Total Other Income
tab_06 <- ESS4_design %>%
  group_by(item) %>% 
  filter(s13q01 == 1) %>%
  summarise(
    M_hat  = survey_mean(income, na.rm = TRUE, vartype = c("se", "cv")),
    Md_hat = survey_median(income, na.rm = TRUE, vartype = c("se", "cv"))
  )

# Estimating proportion of HH that received other income sources
tab_06_P <- ESS4_design %>%
  group_by(item) %>% 
  summarise(P_hat  = survey_mean(yes_no, na.rm = TRUE, vartype = c("se", "cv")))

# Merging income and proportion tables
tab_06 <- inner_join(tab_06, tab_06_P)

# Formatting and displaying results
tab_06 %>%
  transmute(
    item = as_factor(item),
    P_hat = round(P_hat * 100, 2),
    P_hat_cv = round(P_hat_cv * 100, 2),
    P_hat_se = round(P_hat_se * 100, 2),
    Md_hat = round(Md_hat),
    Md_hat_se = round(Md_hat_se),
    Md_hat_cv = round(Md_hat_cv * 100, 2)
  ) 

################################################################################
# The Gini Coefficient.                                                        #
################################################################################

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
HH_data <- HH_data %>% inner_join(EXP_data)

HH_data <- HH_data %>%
  mutate(percapita_expenditure = total_expenditure / saq09)

# Summary statistics
summary(HH_data$percapita_expenditure)

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

design_gini <- convey_prep(ESS4_design)
options(survey.lonely.psu = "fail")
summary(design_gini)

#------------------------------------------------------------------------------#
#          Computing Gini Coefficient based on expenditure                     #
#------------------------------------------------------------------------------#

# National Gini coefficient
tab_07_tot <- svygini(~ percapita_expenditure,
                      design = design_gini) %>%
  as.data.frame() %>%
  mutate(Region = "ETHIOPIA") %>%
  rename(SE = percapita_expenditure)

# Regional Gini coefficient
tab_07_region <- map_df(
  unique(HH_data$saq01),
  ~ svygini( ~ percapita_expenditure, design = design_gini %>%
               filter(saq01 == .x)) %>%
    as.data.frame() %>%
    mutate(Region = as_factor(.x))) %>%
  rename(SE = percapita_expenditure)

# Combining results
tab_07 <- bind_rows(tab_07_region, tab_07_tot) %>%
  transmute(Region,
            gini = round(gini * 100, 2),
            SE = round(SE * 100, 2),)  
rownames(tab_07) <- NULL

tab_07 
