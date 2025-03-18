################################################################################
# 5.4 Working with Weights
################################################################################
# Authors: Andrés Gutiérrez & Stalyn Guerrero
# Economic Commission for Latin America and the Caribbean
# Statistics Division
#
# Description:
# This script processes and calibrates survey weights using different methods,
# including Senate weights, normalized weights, and Pfeffermann’s calibration.
# The calibrated weights are then used in linear models to analyze their impact.
#
# Data Source:
# https:/microdata.worldbank.org/index.php/catalog/3823/data-dictionary
# Version:  1
# Date    :  13-03-2025
################################################################################

#------------------------------------------------------------------------------#
#                           Cleaning R Environment                             #
#------------------------------------------------------------------------------#

# Remove all objects from the environment to avoid conflicts
rm(list = ls())

# Perform garbage collection to free memory
gc()

#------------------------------------------------------------------------------#
#                                Libraries                                     #
#------------------------------------------------------------------------------#

# Load necessary libraries for data manipulation, survey design, and visualization
library(dplyr)       
library(survey)       
library(srvyr)        
library(data.table)   
library(magrittr)     
library(haven)        
library(tidyr)      
library(ggplot2)  
library(broom)

# Ensure dplyr's select function is explicitly used to avoid conflicts
select <- dplyr::select

#------------------------------------------------------------------------------#
#                           Loading Dataset                                    #
#------------------------------------------------------------------------------#

# Load the survey dataset
IND_data_regression <- readRDS("data/data_ESS4/IND_data_regression.rds")

#------------------------------------------------------------------------------#
#                      Weight Calibration and Adjustments                      #
#------------------------------------------------------------------------------#

# Create different weight adjustments:
IND_data_regression <- IND_data_regression %>%
  mutate(
    # Senate Weights: Adjust original survey weights to sum up to the sample size
    pw_Senate = pw_w4 * (n() / sum(pw_w4)),  
    
    # Normalized Weights: Scale original weights so they sum up to 1
    pw_Normalized = pw_w4 / sum(pw_w4)
  )

# Calibrate weights using Pfeffermann’s regression-based method
EXP_model_pw_w4 <- lm(pw_w4 ~ 1 + Zone * Religion + Zone + Sexo + age_group,
                      data = IND_data_regression)

# Predicted values for weight calibration
pw_w4_pred <- predict(EXP_model_pw_w4)

# Apply Pfeffermann’s weight calibration
IND_data_regression <- IND_data_regression %>%
  mutate(pw_Pfeffermann = pw_w4 / pw_w4_pred)

#------------------------------------------------------------------------------#
#                        Descriptive Analysis                                  #
#------------------------------------------------------------------------------#

# Display summary statistics for each weight type
summary(IND_data_regression$pw_Senate)
summary(IND_data_regression$pw_Normalized)
summary(IND_data_regression$pw_Pfeffermann)
summary(IND_data_regression$pw_w4)

# Compare original and calibrated weights using scatter plots
par(mfrow = c(1, 3)) # Set layout for multiple plots

# Plot: Original vs. Senate Weights
plot(
  IND_data_regression$pw_w4,
  IND_data_regression$pw_Senate,
  main = "Original vs. Senate Weights",
  xlab = "Original Weights",
  ylab = "Senate Weights"
)

# Plot: Original vs. Normalized Weights
plot(
  IND_data_regression$pw_w4,
  IND_data_regression$pw_Normalized,
  main = "Original vs. Normalized Weights",
  xlab = "Original Weights",
  ylab = "Normalized Weights"
)

# Plot: Original vs. Pfeffermann Weights
plot(
  IND_data_regression$pw_w4,
  IND_data_regression$pw_Pfeffermann,
  main = "Original vs. Pfeffermann Weights",
  xlab = "Original Weights",
  ylab = "Pfeffermann Weights"
)

#------------------------------------------------------------------------------#
#                          Defining Survey Designs                             #
#------------------------------------------------------------------------------#

# Set options to handle single PSU issues
options(survey.lonely.psu = "fail") 

# Define survey designs using different weights

# Design with original weights
design_sampleing <- IND_data_regression %>%
  filter(percapita_expenditure > 0) %>%
  as_survey_design(
    ids = ea_id,         # Primary sampling unit (PSU)
    strata = strata,     # Stratification variable
    weights = pw_w4,     # Use original weights
    nest = TRUE
  ) %>%
  mutate(log_expenditure = log(percapita_expenditure + 70))

# Design with Senate Weights
design_Senate <- IND_data_regression %>%
  filter(percapita_expenditure > 0) %>%
  as_survey_design(
    ids = ea_id,
    strata = strata,
    weights = pw_Senate, # Use Senate Weights
    nest = TRUE
  ) %>%
  mutate(log_expenditure = log(percapita_expenditure + 70))

# Design with Normalized Weights
design_Normalized <- IND_data_regression %>%
  filter(percapita_expenditure > 0) %>%
  as_survey_design(
    ids = ea_id,
    strata = strata,
    weights = pw_Normalized, # Use Normalized Weights
    nest = TRUE
  ) %>%
  mutate(log_expenditure = log(percapita_expenditure + 70))

# Design with Pfeffermann’s Weights
design_Pfeffermann <- IND_data_regression %>%
  filter(percapita_expenditure > 0) %>%
  as_survey_design(
    ids = ea_id,
    strata = strata,
    weights = pw_Pfeffermann, # Use Pfeffermann Weights
    nest = TRUE
  ) %>%
  mutate(log_expenditure = log(percapita_expenditure + 70))

#------------------------------------------------------------------------------#
#                         Estimating Linear Models                             #
#------------------------------------------------------------------------------#

# Define the regression model formula
formula_model <- as.formula(
  log_expenditure  ~  1 + Zone * Religion + Zone + Sexo + age_group
)

# Estimate models using different survey designs
EXP_model  <- svyglm(formula_model, design = design_sampleing)
EXP_model_Senate  <- svyglm(formula_model, design = design_Senate)
EXP_model_Normalized  <- svyglm(formula_model, design = design_Normalized)
EXP_model_Pfeffermann  <- svyglm(formula_model, design = design_Pfeffermann)

# Display regression results
tidy(EXP_model)
tidy(EXP_model_Senate)
tidy(EXP_model_Normalized)
tidy(EXP_model_Pfeffermann)
