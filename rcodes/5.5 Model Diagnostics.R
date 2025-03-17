################################################################################
# 5.5 Model Diagnostics
################################################################################
# Author: Andrés Gutiérrez, Stalyn Guerrero
#
# Description:
# This script performs model diagnostics for survey-weighted regression models,
# including coefficient of determination, standardized residuals, influential
# observations analysis, and various diagnostic plots.
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
library(svydiags)
library(ggplot2)

# Ensure the correct use of dplyr's select function
select <- dplyr::select

#------------------------------------------------------------------------------#
#                           Loading Dataset                                    #
#------------------------------------------------------------------------------#

data_sec_regression <-
  readRDS("data/data_ESS4/data_sec_regression.rds")

#------------------------------------------------------------------------------#
#                        Defining Survey Design                                #
#------------------------------------------------------------------------------#

options(survey.lonely.psu = "fail")

design_sampling <- data_sec_regression %>%
  filter(percapita_expenditure > 0) %>%
  as_survey_design(
    ids = ea_id,
    strata = strata,
    weights = pw_w4,
    nest = TRUE
  ) %>%
  mutate(log_expenditure = log(percapita_expenditure + 70))

#------------------------------------------------------------------------------#
#                     Estimating Survey-weighted Model                         #
#------------------------------------------------------------------------------#

fit_lm <-
  log_expenditure  ~ -1 + Zone * Religion + Zone + Sexo + age_group


fit_svy <- svyglm(fit_lm, design = design_sampling)

# Display model coefficients
tidy(fit_svy) %>% kable(digits = 2)

#------------------------------------------------------------------------------#
#                     5.5.1 Coefficient of Determination                        #
#------------------------------------------------------------------------------#

modNul <-
  svyglm(log_expenditure ~ 1, design = design_sampling)

s1 <- summary(fit_svy)
s0 <- summary(modNul)

wSST <- s0$dispersion
wSSE <- s1$dispersion

R2 = 1 - wSSE / wSST
R2

#------------------------------------------------------------------------------#
#                     5.5.2 Standardized Residuals                             #
#------------------------------------------------------------------------------#

stdresids <- as.numeric(svystdres(fit_svy)$stdresids)
design_sampling$variables %<>% mutate(stdresids = stdresids)

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
  labs(title = "Standardized Residuals", x = "", y = "Density") +
  theme_minimal(base_size = 20) +
  theme(plot.title = element_text(hjust = 0.5))

# Q-Q Plot
ggplot(data = data.frame(stdresids), aes(sample = stdresids)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot of Standardized Residuals", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal(base_size = 20) +
  theme(plot.title = element_text(hjust = 0.5))


# Checking homoscedasticity (constant variance of errors)
ggplot(data = data.frame(stdresids, fitted = fit_svy$fitted.values),
       aes(x = fitted, y = stdresids)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0,
             color = "red",
             linetype = "dashed") +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values", 
       y = "Standardized Residuals") +
  theme_minimal(base_size = 20) +
  theme(plot.title = element_text(hjust = 0.5))

#------------------------------------------------------------------------------#
#                     5.5.3 Influential Observations Analysis                  #
#------------------------------------------------------------------------------#

## Cook's Distance Analysis
d_cook = data.frame(cook = svyCooksD(fit_svy),
                    id = 1:length(svyCooksD(fit_svy)))

d_cook %<>% mutate(Criterion = ifelse(cook > 3, "Yes", "No"))

ggplot(d_cook, aes(y = cook, x = id)) +
  geom_point(aes(col = Criterion)) +
  geom_hline(aes(yintercept = 3),
             color = "black",
             linetype = "dashed") +
  scale_color_manual(values = c("Yes" = "red", "No" = "black")) +
  labs(title = "Cook's Distance") +
  theme_minimal(base_size = 20) +
  theme(plot.title = element_text(hjust = 0.5))

## DFBETAS Analysis
# Compute DFBETAS for each observation
d_dfbetas <- svydfbetas(fit_svy)$Dfbetas %>%
  t() %>%
  as.data.frame()

# Rename columns for clarity
colnames(d_dfbetas)[1:(ncol(d_dfbetas))] <-
  paste0("beta[", seq_len(ncol(d_dfbetas)) - 1, "]")

# Reshape the data to long format for visualization
d_dfbetas_long <- d_dfbetas %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id,
               names_to = "Variable",
               values_to = "value")

# Define cutoff value for influential observations
cutoff <- svydfbetas(fit_svy)$cutoff

d_dfbetas_long <- d_dfbetas_long %>%
  mutate(Criterion = ifelse(abs(value) > cutoff, "Yes", "No"))


# Plot DFBETAS values for each observation
ggplot(d_dfbetas_long , aes(y = abs(value), x = id)) +
  geom_point(aes(color = Criterion)) +
  geom_hline(aes(yintercept = cutoff), linetype = "dashed") +
  facet_wrap(. ~ Variable, nrow = 4, scales = "free_y") +
  scale_color_manual(values = c("Yes" = "red", "No" = "black")) +
  labs(title = "DFBETAS Analysis", 
       x = "Observation ID", 
       y = expression(abs(beta))) +
  theme_minimal(base_size = 20) +
  theme(plot.title = element_text(hjust = 0.5))

## DFFITS Analysis
d_dffits <-
  data.frame(dffits = svydffits(fit_svy)$Dffits,
             id = 1:length(svydffits(fit_svy)$Dffits))
cutoff <- svydffits(fit_svy)$cutoff

d_dffits <-
  d_dffits %>%
  mutate(Criterion = ifelse(abs(dffits) > cutoff, "Yes", "No"))

ggplot(d_dffits, aes(y = abs(dffits), x = id)) +
  geom_point(aes(color = Criterion)) +
  scale_color_manual(values = c("Yes" = "red", "No" = "black")) +
  geom_hline(yintercept = cutoff,
             linetype = "dashed",
             color = "black") +
  labs(title = "DFFITS Influence Analysis", x = "Observation ID", y = expression(abs(DFFITS))) +
  theme_minimal(base_size = 20) +
  theme(plot.title = element_text(hjust = 0.5))
