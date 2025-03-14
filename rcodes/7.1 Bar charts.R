################################################################################
# 7.1 Bar charts
################################################################################
# Author: Andrés Gutiérrez, Stalyn Guerrero
# 
# Description:
# This section generates bar charts to visualize household spending on 
# non-food items and services in Ethiopia, based on survey-weighted data. 
# It estimates:
# 1. The total number of households that spent on non-food items.
# 2. The total expenditure on non-food items.
# 3. The proportion of households that incurred these expenses.
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
library(broom)
library(ggplot2)
library(scales)
library(forcats)

#------------------------------------------------------------------------------#
#                           Loading Datasets                                   #
#------------------------------------------------------------------------------#

options(survey.lonely.psu = "fail")
data_sec <- read_sav("data/data_ESS4/sect7b_hh_w4_v2.sav")

#------------------------------------------------------------------------------#
#                             7.1 Bar Charts                                   #
#------------------------------------------------------------------------------#

# Prepare dataset:
# - Group by item category
# - Convert missing or non-reported expenditures to zero
# - Create an indicator variable (yes/no) for whether a household spent on an item

data_sec <- data_sec %>%
  group_by(item = item_cd_12months) %>%
  mutate(expenditure = ifelse(s7q03 == 2, 0, s7q04),  # If no expense, set to 0
         yes_no = ifelse(s7q03 == 2, 0, 1))           # Indicator for spending (1) or not (0)

# Defining survey design

design_sampling <- data_sec %>%
  mutate(strata = paste0(saq01, "_", saq14)) %>%
  as_survey_design(
    ids = ea_id,
    strata = strata,
    weights = pw_w4,
    nest = TRUE
  )

#------------------------------------------------------------------------------#
# TABLE 7.3 - Spending on Nonfood Items and Services
#------------------------------------------------------------------------------#
# Compute weighted estimates:
# - `N_hat`: Total number of households spending on an item
# - `P_hat`: Proportion of households spending on an item
# - `T_hat`: Total amount spent on each item

tab_03 <- design_sampling %>% summarise(
  N_hat = survey_total(yes_no, na.rm = TRUE, vartype = c("se", "ci")),
  P_hat = survey_mean(yes_no, na.rm = TRUE, vartype = c("se", "ci")),
  T_hat = survey_total(expenditure, na.rm = TRUE, vartype = c("se", "ci"))
) %>% mutate(item = as_factor(item))

#------------------------------------------------------------------------------#
# Bar Chart for Estimated Number of Households Spending on Non-Food Items
#------------------------------------------------------------------------------#


tab_03 <-
  tab_03 %>% mutate(item = fct_reorder(item, N_hat, .desc = FALSE))

ggplot(data = tab_03, aes(
  x = item,
  y = N_hat,
  ymax = N_hat_upp,
  ymin = N_hat_low
)) +
  geom_bar(stat = "identity",
           position = "dodge",
           fill = "#CCE5FF") +
  geom_errorbar(position = position_dodge(width = 0.9), width = 0.3) +
  scale_y_continuous(labels = label_number(accuracy = 1)) +
  labs(
    y = expression(hat(N)),
    x = "",
    title = "Estimated number of households spending on non-food items and
       services in the previous year, Ethiopia 2018/19"
  ) +
  theme_minimal(20) +
  coord_flip()

#------------------------------------------------------------------------------#
# Bar Chart for Estimated Total Household Expenditure on Non-Food Items
#------------------------------------------------------------------------------#

tab_03 <- tab_03 %>% mutate(item = fct_reorder(item, T_hat, .desc = FALSE))

ggplot(data = tab_03, aes(
  x = item,
  y = T_hat,
  ymax = T_hat_upp,
  ymin = T_hat_low
)) +
  geom_bar(stat = "identity",
           position = "dodge",
           fill = "#CCE5FF") +
  geom_errorbar(position = position_dodge(width = 0.9), width = 0.3) +  # Error bars for confidence intervals
  scale_y_continuous(labels = label_number(accuracy = 1)) +  # Format y-axis labels
  labs(
    y = expression(hat(T)),  # LaTeX-style notation for total expenditure
    x = "",
    title = "Estimated total household expenditure on non-food items and
       services in the previous year, Ethiopia 2018/19"
  ) +
  theme_minimal(20) +
  coord_flip()

#------------------------------------------------------------------------------#
# Bar Chart for Estimated Proportion of Households Spending on Non-Food Items
#------------------------------------------------------------------------------#

tab_03 <-
  tab_03 %>% mutate(item = fct_reorder(item, P_hat, .desc = FALSE))

ggplot(data = tab_03, aes(
  x = item,
  y = P_hat,
  ymax = P_hat_upp,
  ymin = P_hat_low
)) +
  geom_bar(stat = "identity",
           position = "dodge",
           fill = "#CCE5FF") +
  geom_errorbar(position = position_dodge(width = 0.9), width = 0.3) +
  scale_y_continuous(labels = label_number(accuracy = 0.05)) +
  labs(
    y = expression(hat(P)),
    x = "",
    title = "Estimated proportion of households that spent on non-food items
       and services in the previous year, Ethiopia 2018/19"
  ) +
  theme_minimal(20) +
  coord_flip()
