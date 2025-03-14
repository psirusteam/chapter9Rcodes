################################################################################
# 5.4 Working with Weights
################################################################################
# Author: Andrés Gutiérrez
#
# Description:
# This script processes and calibrates survey weights using different methods,
# including Senate weights, normalized weights, and Pfeffermann’s calibration.
# It then applies these weights in linear models to analyze their impact.
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

# Ensure the correct use of dplyr's select function
select <- dplyr::select

#------------------------------------------------------------------------------#
#                           Loading Dataset                                    #
#------------------------------------------------------------------------------#

data_sec_regression <-
  readRDS("data/data_ESS4/data_sec_regression.rds")

#------------------------------------------------------------------------------#
#                      Weight Calibration and Adjustments                      #
#------------------------------------------------------------------------------#

# Creating different survey weights
data_sec_regression <- data_sec_regression %>%
  mutate(pw_Senate = pw_w4 * (n() / sum(pw_w4)),
         pw_Normalized = (pw_w4 / sum(pw_w4)))

# Calibrating weights using Pfeffermann’s method
modwk <- lm(pw_w4 ~ Zone * Religion + Zone + Sexo + age_group,
            data = data_sec_regression)

wkpred <- predict(modwk)

data_sec_regression <- data_sec_regression %>%
  mutate(pw_Pfeffermann = pw_w4 / wkpred)

#------------------------------------------------------------------------------#
#                        Descriptive Analysis                                  #
#------------------------------------------------------------------------------#

# Summary statistics for different weights
summary(data_sec_regression$pw_Senate)
summary(data_sec_regression$pw_Normalized)
summary(data_sec_regression$pw_Pfeffermann)
summary(data_sec_regression$pw_w4)

# Scatter plots comparing original and calibrated weights
par(mfrow = c(1, 3))
plot(
  data_sec_regression$pw_w4,
  data_sec_regression$pw_Senate,
  main = "Original vs. Senate Weights",
  xlab = "Original Weights",
  ylab = "Senate Weights"
)

plot(
  data_sec_regression$pw_w4,
  data_sec_regression$pw_Normalized,
  main = "Original vs. Normalized Weights",
  xlab = "Original Weights",
  ylab = "Normalized Weights"
)

plot(
  data_sec_regression$pw_w4,
  data_sec_regression$pw_Pfeffermann,
  main = "Original vs. Pfeffermann Weights",
  xlab = "Original Weights",
  ylab = "Pfeffermann Weights"
)

#------------------------------------------------------------------------------#
#                          Defining Survey Designs                             #
#------------------------------------------------------------------------------#

options(survey.lonely.psu = "fail") 

# Survey designs using different weights
design_sampleing <- data_sec_regression %>%
  filter(percapita_expenditure > 0) %>%
  as_survey_design(
    ids = ea_id,
    strata = strata,
    weights = pw_w4, # "Original Weights",
    nest = TRUE
  ) %>%
  mutate(log_expenditure = log(percapita_expenditure + 70))


design_Senate <- data_sec_regression %>%
  filter(percapita_expenditure > 0) %>%
  as_survey_design(
    ids = ea_id,
    strata = strata,
    weights = pw_Senate, # "Senate Weights"
    nest = TRUE
  ) %>%
  mutate(log_expenditure = log(percapita_expenditure + 70))


design_Normalized <- data_sec_regression %>%
  filter(percapita_expenditure > 0) %>%
  as_survey_design(
    ids = ea_id,
    strata = strata,
    weights = pw_Normalized, # "Normalized Weights"
    nest = TRUE
  ) %>%
  mutate(log_expenditure = log(percapita_expenditure + 70))


design_Pfeffermann <- data_sec_regression %>%
  filter(percapita_expenditure > 0) %>%
  as_survey_design(
    ids = ea_id,
    strata = strata,
    weights = pw_Pfeffermann, # "Pfeffermann Weights"
    nest = TRUE
  ) %>%
  mutate(log_expenditure = log(percapita_expenditure + 70))

#------------------------------------------------------------------------------#
#                         Estimating Linear Models                             #
#------------------------------------------------------------------------------#

# Defining model formula
fit_lm <-
  as.formula(log_expenditure  ~ -1 + Zone * Religion + Zone + Sexo + age_group)

# Running models with different weights
fit_svy  <- svyglm(fit_lm, design = design_sampleing)
fit_Senate  <- svyglm(fit_lm, design = design_Senate)
fit_Normalized  <- svyglm(fit_lm, design = design_Normalized)
fit_Pfeffermann  <- svyglm(fit_lm, design = design_Pfeffermann)

# Displaying results
tidy(fit_svy) %>% kable(digits = 2)
tidy(fit_Senate) %>% kable(digits = 2)
tidy(fit_Normalized) %>% kable(digits = 2)
tidy(fit_Pfeffermann) %>% kable(digits = 2)
