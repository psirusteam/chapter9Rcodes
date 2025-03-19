################################################################################
# 5.5 Model Diagnostics                                                        #
################################################################################
# Authors: Andrés Gutiérrez & Stalyn Guerrero
# Economic Comission for Latin America and the Caribbean 
# Statistics Division
#
# Description:
# This script performs diagnostics on survey-weighted regression models, 
# evaluating key aspects such as:
# - Coefficient of determination (R²)
# - Standardized residuals analysis
# - Identification of influential observations
# - Various diagnostic plots for model validation
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

# Load required packages for data handling, survey analysis, and diagnostics

library(dplyr)        
library(survey)       
library(srvyr)       
library(data.table)  
library(magrittr)     
library(haven)        
library(tidyr)        
library(svydiags)     # Survey diagnostics
library(ggplot2)      
library(broom)       

# Ensure dplyr's select function is correctly used to avoid conflicts
select <- dplyr::select

#------------------------------------------------------------------------------#
#                           Loading Dataset                                    #
#------------------------------------------------------------------------------#

# Load the dataset containing survey information
IND_data_regression <- readRDS("data/data_ESS4/IND_data_regression.rds")

#------------------------------------------------------------------------------#
#                        Defining Survey Design                                #
#------------------------------------------------------------------------------#

# Set survey options to avoid errors in single PSU strata
options(survey.lonely.psu = "fail")

# Define the survey design object using weighted data
ESS4_design <- IND_data_regression %>%
  filter(percapita_expenditure > 0) %>%
  as_survey_design(
    ids = ea_id,         # Primary Sampling Units (PSU)
    strata = strata,     # Stratification variable
    weights = pw_w4,     # Sampling weights
    nest = TRUE
  ) %>%
  mutate(log_expenditure = log(percapita_expenditure + 70)) # Log transformation

#------------------------------------------------------------------------------#
#                     Estimating Survey-weighted Model                         #
#------------------------------------------------------------------------------#

# Define and estimate a survey-weighted regression model
EXP_model <- svyglm(
  log_expenditure ~ 1 + Zone * Religion + Zone + Sex + age_group,
  design = ESS4_design
)

# Display model summary and coefficients
summary(EXP_model)
tidy(EXP_model)

#------------------------------------------------------------------------------#
#                   Model Fit: R² Calculation                                  #
#------------------------------------------------------------------------------#

# Fit a null model (intercept-only) for R² computation
null_model <- svyglm(log(percapita_expenditure + 1) ~ 1, design = ESS4_design)

# Extract residual dispersion values
s1 <- summary(EXP_model)
s0 <- summary(null_model)

# Compute weighted R² using total variance and residual variance
wSST <- s0$dispersion  # Total variance
wSSE <- s1$dispersion  # Residual variance

R2 <- 1 - (wSSE / wSST)  # R² formula
R2  # Display computed R² value

#------------------------------------------------------------------------------#
#                     Standardized Residuals Analysis                          #
#------------------------------------------------------------------------------#

# Compute standardized residuals from the model
stdresids <- as.numeric(svystdres(EXP_model)$stdresids)

# Attach residuals to the dataset
# ESS4_design$variables %<>% mutate(stdresids = stdresids)

# Histogram of standardized residuals
ggplot(data = data.frame(stdresids), aes(x = stdresids)) +
  geom_histogram(
    aes(y = ..density..),
    bins = 30,
    fill = "#CCE5FF",
    color = "black"
  ) +
  stat_function(
    fun = dnorm,
    args = list(mean = 0, sd = 1),
    color = "black",
    size = 1
  ) +
  labs(title = "Standardized Residuals", x = "Residuals", y = "Density") +
  theme_minimal(base_size = 20) +
  theme(plot.title = element_text(hjust = 0.5))

# Q-Q plot to assess normality of residuals
ggplot(data = data.frame(stdresids), aes(sample = stdresids)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot of Standardized Residuals",
       x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal(base_size = 20)+
  theme(plot.title = element_text(hjust = 0.5))

#------------------------------------------------------------------------------#
#                         Influential Observations                             #
#------------------------------------------------------------------------------#

#---------------------#
# Cook's Distance     #
#---------------------#

# Compute Cook's Distance and flag influential observations
CooksD <- data.frame(
  cook = svyCooksD(EXP_model),
  id = 1:length(svyCooksD(EXP_model))
) %>%
  mutate(Criterion = ifelse(cook > 3, "Yes", "No"))

# Scatter plot of Cook's Distance
ggplot(CooksD, aes(y = cook, x = id)) +
  geom_point(aes(col = Criterion)) +
  geom_hline(aes(yintercept = 3), color = "black", linetype = "dashed") +
  scale_color_manual(values = c("Yes" = "red", "No" = "black")) +
  theme_minimal(base_size = 20) +
  labs(title = "Cook's Distance", x = "Observation ID", y = "Cook's D")

#---------------------#
# DFBETAS Analysis    #
#---------------------#

# Compute DFBETAS for all model coefficients
d_dfbetas <- svydfbetas(EXP_model)$Dfbetas %>%
  t() %>%
  as.data.frame()

# Rename DFBETAS columns
colnames(d_dfbetas) <- paste0("beta[", seq_len(ncol(d_dfbetas)) - 1, "]")

# Convert DFBETAS to long format
d_dfbetas_long <- d_dfbetas %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "Variable", values_to = "value")

# Get threshold for influential observations
cutoff <- svydfbetas(EXP_model)$cutoff

# Flag influential observations
d_dfbetas_long <- d_dfbetas_long %>%
  mutate(Criterion = ifelse(abs(value) > cutoff, "Yes", "No"))

# Plot DFBETAS
ggplot(d_dfbetas_long ,
       aes(y = abs(value), x = id)) +
  geom_point(aes(color = Criterion)) +
  geom_hline(aes(yintercept = cutoff), linetype = "dashed") +
  facet_wrap(. ~ Variable, nrow = 4, scales = "free_y") +
  scale_color_manual(values = c("Yes" = "red", "No" = "black")) +
  labs(title = "DFBETAS Analysis", x = "Observation ID", y = expression(abs(beta))) +
  theme_minimal(base_size = 20)

#---------------------#
# DFFITS Analysis     #
#---------------------#

# Compute DFFITS values
d_dffits <- data.frame(
  dffits = svydffits(EXP_model)$Dffits,
  id = 1:length(svydffits(EXP_model)$Dffits)
)

# Get influence threshold
cutoff <- svydffits(EXP_model)$cutoff

# Flag influential observations
d_dffits <- d_dffits %>%
  mutate(Criterion = ifelse(abs(dffits) > cutoff, "Yes", "No"))

# Plot DFFITS values
ggplot(d_dffits, aes(y = abs(dffits), x = id)) +
  geom_point(aes(color = Criterion)) +
  geom_hline(yintercept = cutoff, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("Yes" = "red", "No" = "black")) +
  labs(title = "DFFITS Influence Analysis", x = "Observation ID", y = expression(abs(DFFITS))) +
  theme_minimal(base_size = 20)

