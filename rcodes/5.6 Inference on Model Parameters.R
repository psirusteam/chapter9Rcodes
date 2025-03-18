################################################################################
# 5.6 Inference on Model Parameters                                            #
################################################################################
# Authors: Andrés Gutiérrez & Stalyn Guerrero
# Economic Commission for Latin America and the Caribbean
# Statistics Division
# 
# Description:
# This script conducts inference on model parameters for survey-weighted 
# regression models, including:
# - Estimation of model coefficients
# - Calculation of confidence intervals
# - Visualization of coefficients with confidence intervals
# - Comparison of predicted vs observed values
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
library(svydiags)
library(ggplot2)
library(broom)

# Ensure the correct use of dplyr's select function
select <- dplyr::select

#------------------------------------------------------------------------------#
#                           Loading Datasets                                   #
#------------------------------------------------------------------------------#

IND_data_regression <- readRDS("data/data_ESS4/IND_data_regression.rds")

#------------------------------------------------------------------------------#
# Defining the survey design for weighted regression
#------------------------------------------------------------------------------#

# Set survey options to handle single PSU issues
options(survey.lonely.psu = "fail") 

# Define survey design with appropriate stratification and weighting
ESS4_design <- IND_data_regression %>%
  filter(percapita_expenditure > 0)   %>% 
  as_survey_design(
    ids = ea_id,       # Primary Sampling Unit (PSU)
    strata = strata,   # Stratification variable
    weights = pw_w4,   # Survey weights
    nest = TRUE
  ) %>% 
  mutate(log_expenditure = log(percapita_expenditure + 70)) # Log transformation for normality


#------------------------------------------------------------------------------#
#                         Estimating Linear Models                             #
#------------------------------------------------------------------------------#

# Survey-weighted linear model for log expenditure
EXP_model <-
  svyglm(log_expenditure  ~ 1 + Zone * Religion + Zone + Sexo + age_group,
         design = ESS4_design)

# Display model coefficients
summary(EXP_model)
EXP_model %>% coef()

#------------------------------------------------------------------------------#
#                    Inference on Model Parameters                             #
#------------------------------------------------------------------------------#

# Extract coefficients and confidence intervals
coef_df <- tidy(EXP_model, conf.int = TRUE)


# Define factor labels for coefficients
coef_df$beta <- factor(
  paste0("beta[", 1:nrow(coef_df) -1, "]"),
  ordered = TRUE,
  levels = paste0("beta[", 1:nrow(coef_df) - 1, "]"),
  labels = paste0("beta[", 1:nrow(coef_df) - 1, "]")
)


# Generate LaTeX-like expressions for beta coefficients
beta_labels <- setNames(
  lapply(0:(nrow(coef_df) - 1), function(i) bquote(beta[.(i)])),
  paste0("beta[", 0:(nrow(coef_df) - 1), "]")
)


# Plot model coefficients with confidence intervals
ggplot(coef_df, aes(x = beta, y = estimate)) +
  geom_point(size = 3, color = "blue") +  # Point estimates
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2,
                color = "black") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +  # Zero-reference line
  labs(
    title = "Survey Model Coefficients",
    subtitle = "Point estimates with 95% confidence intervals",
    x = "Coefficients",
    y = "Estimate"
  ) +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_x_discrete(labels = beta_labels) +  # Use LaTeX expressions for labels
  coord_flip()  # Flip axes for readability


#------------------------------------------------------------------------------#
#                     Comparison: Predicted vs Observed Values                 #
#------------------------------------------------------------------------------#

# Extract predicted values from the model
predicted_values <- predict(EXP_model) %>% data.frame()

# Extract observed values from the dataset
observed_values <- ESS4_design$variables$log_expenditure

# Create scatter plot of observed vs predicted values
ggplot(data = data.frame(observed = observed_values, predicted = predicted_values[,1]), 
       aes(x = observed, y = predicted)) +
  geom_point(alpha = 0.5, color = "blue") +  # Scatter plot with transparency
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # Reference line
  labs(
    title = "Observed vs Predicted Values",
    x = "Observed Log Expenditure",
    y = "Predicted Log Expenditure"
  ) +
  theme_minimal(base_size = 20) +
  theme(plot.title = element_text(hjust = 0.5))