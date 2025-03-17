################################################################################
# 3.4 Percentiles and inequality measures
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

data_sec <- read_sav("data/data_ESS4/sect13_hh_w4_v2.sav")

#------------------------------------------------------------------------------#
#                Processing Income Data                                        #
#------------------------------------------------------------------------------#

data_sec <- data_sec %>% 
  group_by(item = source_cd) %>%
  mutate(
    income = ifelse(s13q01 == 2, 0, s13q02),
    yes_no = ifelse(s13q01 == 2, 0, 1)
  )

#------------------------------------------------------------------------------#
# Defining Survey Design                                                       #
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


# Estimating mean and median income
tab_06 <- design_sampling %>% 
  filter(s13q01 == 1) %>% 
  summarise(
    M_hat  = survey_mean(income, na.rm = TRUE, vartype = c("se", "cv")),
    Md_hat = survey_median(income, na.rm = TRUE, vartype = c("se", "cv"))
  )

# Estimating proportion of income sources
tab_06_P <- design_sampling %>% 
  summarise(
    P_hat  = survey_mean(yes_no, na.rm = TRUE, vartype = c("se", "cv"))
  )

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
  ) %>% 
  kable(digits = 2)

################################################################################
# The Gini Coefficient
################################################################################

#------------------------------------------------------------------------------#
#                           Loading Datasets                                   #
#------------------------------------------------------------------------------#

data_expenditure <- read_sav("data/data_ESS4/sect7b_hh_w4_v2.sav") %>%
  mutate(expenditure = ifelse(s7q03 == 2, 0, s7q04)) %>%
  group_by(household_id) %>%
  summarise(total_expenditure = sum(expenditure, na.rm = TRUE))

data_sec <- read_sav("data/data_ESS4/sect_cover_hh_w4.sav")

#------------------------------------------------------------------------------#
#                      Processing Per Capita Expenditure                       #
#------------------------------------------------------------------------------#

# Merging household data with expenditure data
data_sec <- data_sec %>% inner_join(data_expenditure)

data_sec <- data_sec %>% 
  mutate(percapita_expenditure = total_expenditure / saq09)

# Summary statistics
summary(data_sec$percapita_expenditure)

#------------------------------------------------------------------------------#
#                          Defining Survey Design                              #
#------------------------------------------------------------------------------#

design_sampling <- data_sec %>% 
  mutate(strata = paste0(saq01, "_", saq14)) %>% 
  as_survey_design(
    ids = ea_id,  # Primary sampling unit identifier (EA)
    strata = strata, # Stratification by region (saq01) and urban/rural zone (saq14)
    weights = pw_w4,  # Final adjusted weight
    nest = TRUE
  )

design_gini <- convey_prep(design_sampling)

#------------------------------------------------------------------------------#
#                         Computing Gini Coefficient                           #
#------------------------------------------------------------------------------#

# National Gini coefficient
tab_07_tot <- svygini(~ percapita_expenditure,
                      design = design_gini) %>%
  as.data.frame() %>%
  mutate(Region = "Ethiopia") %>% 
  rename(SE = percapita_expenditure)

# Regional Gini coefficient
tab_07_region <- map_df(
  unique(data_sec$saq01),
  ~ svygini( ~ percapita_expenditure, design = design_gini %>%
               filter(saq01 == .x)) %>%
    as.data.frame() %>%
    mutate(Region = as_factor(.x))
) %>%
  rename(SE = percapita_expenditure)

# Combining results
tab_07 <- bind_rows(tab_07_region, tab_07_tot)
rownames(tab_07) <- NULL

tab_07 %>%
  transmute(
    Region, 
    gini = round(gini*100, 2),
    SE = round(SE*100, 2),
  )  %>%   kable(digits = 2)

