#------------------------------------------------------------------------------#
# 5.6 Inference on Model Parameters
#------------------------------------------------------------------------------#
# Author: Andrés Gutiérrez, Stalyn Guerrero
# 
# Description:
# This script conducts inference on model parameters for survey-weighted regression
# models, including coefficient estimation, confidence intervals, and a comparison
# between predicted and observed values.
# 
# Data Source:
# https:/microdata.worldbank.org/index.php/catalog/3823/data-dictionary
# Version:  1
# Date    :  13-03-2025
#------------------------------------------------------------------------------#

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
library(svydiags)
library(ggplot2)

# Ensure the correct use of dplyr's select function
select <- dplyr::select

#------------------------------------------------------------------------------#
#                           Loading Datasets                                   #
#------------------------------------------------------------------------------#

data_sec_regression <- readRDS("data/data_ESS4/data_sec_regression.rds")

#------------------------------------------------------------------------------#
# Defining the survey design for weighted regression
#------------------------------------------------------------------------------#

options(survey.lonely.psu = "fail") 

design_sampling <- data_sec_regression %>%
  filter(percapita_expenditure > 0)   %>% 
  as_survey_design(
    ids = ea_id,  # Primary sampling unit identifier (EA)
    strata = strata,  # Stratification by region (saq01) and urban/rural zone (saq14)
    weights = pw_w4,  # Final adjusted weight
    nest = TRUE
  ) %>% 
  mutate(log_expenditure = log(percapita_expenditure + 70))

#------------------------------------------------------------------------------#
# Fitting the survey-weighted regression model
#------------------------------------------------------------------------------#

fit_lm <- as.formula(log_expenditure  ~ -1 + Zone * Religion + Zone + Sexo + age_group)

fit_svy  <-   svyglm(fit_lm, design = design_sampling)

# Display model coefficients
tidy(fit_svy) %>% kable(digits = 2)

#------------------------------------------------------------------------------#
# Inference on Model Parameters
#------------------------------------------------------------------------------#

# Extracting coefficients and confidence intervals
coef_df <- tidy(fit_svy, conf.int = TRUE)

# Visualization of coefficient estimates and confidence intervals
ggplot(coef_df, aes(x = reorder(term, estimate), y = estimate)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Survey Model Coefficients",
    subtitle = "Point estimates with 95% confidence intervals",
    x = "Coefficients",
    y = "Estimate"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center title
    plot.subtitle = element_text(hjust = 0.5)  # Center subtitle
  ) +
  coord_flip()  # Flip for better readability

#------------------------------------------------------------------------------#
# Predicted vs Observed Values
#------------------------------------------------------------------------------#

# Extracting predicted values
predicted_values <- predict(fit_svy) %>% data.frame()
observed_values <- design_sampling$variables$log_expenditure

# Scatter plot comparing observed and predicted values
ggplot(data = data.frame(observed = observed_values,
                         predicted = predicted_values[,1]), 
       aes(x = observed, y = predicted)) +
  geom_point(alpha = 0.5, color = "blue") +
  labs(
    title = "Observed vs Predicted Values",
    x = "Observed Log Expenditure",
    y = "Predicted Log Expenditure"
  ) +
  theme_minimal(base_size = 20) +
  theme(plot.title = element_text(hjust = 0.5))
