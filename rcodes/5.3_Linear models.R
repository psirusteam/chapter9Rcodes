################################################################################
# 5.3 Linear Models
################################################################################
# Author: Andrés Gutiérrez, Stalyn Guerrero
# 
# Description:
# This script loads and prepares household survey data, including expenditure
# calculations, demographic characteristics, and survey weighting adjustments,
# followed by the estimation of linear models.
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
#                           Loading Datasets                                   #
#------------------------------------------------------------------------------#

## Household expenditure data
data_expenditure <- read_sav("data/data_ESS4/sect7b_hh_w4_v2.sav") %>%
  mutate(expenditure = ifelse(s7q03 == 2, 0, s7q04)) %>%
  group_by(household_id) %>%
  summarise(total_expenditure = sum(expenditure, na.rm = TRUE))

## General household information
data_sec <- read_sav("data/data_ESS4/sect1_hh_w4.sav")

#------------------------------------------------------------------------------#
#                     Processing Demographic Information                        #
#------------------------------------------------------------------------------#

## Age grouping and demographic characteristics
data_sec_age <- data_sec %>% 
  transmute(
    household_id,
    individual_id,
    ea_id,  # Primary sampling unit identifier (EA)
    pw_w4,
    saq01, # Region
    Zone = as_factor(saq14), # Urban/Rural classification
    strata = paste0(saq01, "_", saq14), # Stratification region-zone
    s1q03a, # Age in years
    age_group = case_when(
      s1q03a < 18 ~ NA_character_,
      s1q03a < 31 ~ "18-30",
      s1q03a < 46 ~ "31-45", 
      s1q03a < 66 ~ "46-65", 
      TRUE ~ "66 or more"
    ),
    Sexo = as_factor(s1q02),
    Religion = as_factor(s1q08) %>% 
      recode(
        "6. PEGAN" = "OTHER", 
        "5. TRADITIONAL" = "OTHER",
        "8. OTHER (SPECIFY)" = "OTHER", 
        "7. WAKEFETA" = "OTHER"
      ) %>% 
      replace_na("OTHER")
  )

#------------------------------------------------------------------------------#
#                      Processing Marital Status                               #
#------------------------------------------------------------------------------#

data_sec1 <- read_sav("data/data_ESS4/sect1_hh_w4.sav")

data_sec1_MARRIED <- data_sec1 %>% 
  transmute(
    household_id,
    individual_id,
    s1q09 = as.character(as_factor(s1q09)) %>% 
      replace_na("not answered")
  )

#------------------------------------------------------------------------------#
#                     Household Expenditure Per Capita                         #
#------------------------------------------------------------------------------#

data_sec2 <- read_sav("data/data_ESS4/sect_cover_hh_w4.sav") %>%
  inner_join(data_expenditure, by = "household_id")

data_sec_expenditure <- data_sec2 %>% 
  transmute(
    household_id,
    percapita_expenditure = total_expenditure / saq09
  )

#------------------------------------------------------------------------------#
#                 Merging Datasets for Regression Analysis                     #
#------------------------------------------------------------------------------#

data_sec_regression <- data_sec_age %>%
  inner_join(data_sec1_MARRIED, by = c("household_id", "individual_id")) %>%
  inner_join(data_sec_expenditure, by = "household_id") %>%
  filter(s1q03a >= 18)

# Saving processed dataset
saveRDS(data_sec_regression, "data/data_ESS4/data_sec_regression.rds")

#------------------------------------------------------------------------------#
#                          Defining Survey Design                              #
#------------------------------------------------------------------------------#

options(survey.lonely.psu = "fail") # Adjust to handle singleton PSUs

design_sampling <- data_sec_regression %>%
  filter(percapita_expenditure > 0) %>% 
  as_survey_design(
    ids = ea_id,  # Primary sampling unit identifier (EA)
    strata = strata, # Stratification by region (saq01) and urban/rural zone (Zone)
    weights = pw_w4,  # Final adjusted weight
    nest = TRUE
  ) %>% 
  mutate(log_expenditure = log(percapita_expenditure + 70))

#------------------------------------------------------------------------------#
#                         Estimating Linear Models                             #
#------------------------------------------------------------------------------#

# Survey-weighted linear model for log expenditure
fit_svy <- svyglm(log_expenditure  ~ -1 + Zone * Religion + Zone + Sexo + age_group,
                  design = design_sampling)

# Display model coefficients
fit_svy %>% coef()


#############################
# Coefficient of determination pendiente. 
#############################

modNul <-
  svyglm(log(percapita_expenditure+1)  ~ 1,
         design = design_sampleing)

s1 <- summary(fit_svy)

s0 <- summary(modNul)

wSST <- s0$dispersion
wSSE <- s1$dispersion

R2 = 1 - wSSE / wSST
R2


########################################
## 5.5.2 Standardized Residuals
########################################
library(svydiags)
stdresids <- as.numeric(svystdres(fit_svy)$stdresids)
design_sampleing$variables %<>% mutate(stdresids = stdresids)

qqnorm(stdresids)
qqline(stdresids, col = 2)

library(ggplot2)

ggplot(data = data.frame(stdresids), aes(x = stdresids)) +
  geom_histogram(
    aes(y = ..density..),
    bins = 30,
    fill = "#CCE5FF",
    color = "black"
  ) +  # Histograma con densidad
  stat_function(
    fun = dnorm,
    args = list(mean = 0, sd = 1),
    color = "black",
    size = 1
  ) + 
  labs(title = "Standardized Residuals",
       x = "",
       y = "") +  # Agrega etiqueta en el eje Y para densidad
  theme_minimal(base_size = 20) +
  theme(plot.title = element_text(hjust = 0.5))  # Centra el título


ggplot(data = data.frame(stdresids), aes(sample = stdresids)) +
  stat_qq() +
  stat_qq_line(color = "red") +  # Reference line in red
  labs(title = "Q-Q Plot of Standardized Residuals",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal(base_size = 20) +
  theme(plot.title = element_text(hjust = 0.5))  # Centers the title

########################################
# Influential Observations
########################################

## Cook's Distance

d_cook = data.frame(cook = svyCooksD(fit_svy),
                    id = 1:length(svyCooksD(fit_svy)))

d_cook %<>%
  mutate(Criterion = ifelse(cook > 3, "Yes", "No"))  # Renamed "Criterio" to "Criterion"

ggplot(d_cook, aes(y = cook, x = id)) +
  geom_point(aes(col = Criterion)) +
  geom_hline(aes(yintercept = 3),
             color = "black",
             linetype = "dashed") +  # Horizontal line at 3
  scale_color_manual(values = c("Yes" = "red", "No" = "black")) +
  scale_y_continuous(breaks = c(0, 3, 6, 9, 12, 15)) +  # Ensure 3 appears on the axis
  theme_minimal(base_size = 20)+ 
  theme(plot.title = element_text(hjust = 0.5)) +  # Centers the title
  labs(title = "Cook's Distance")


# Calculate DFBETAS
d_dfbetas <- svydfbetas(fit_svy)$Dfbetas %>%
  t() %>%
  as.data.frame() 

# Rename Beta columns with LaTeX subscripts
colnames(d_dfbetas)[1:(ncol(d_dfbetas))] <-
  paste0("beta[", seq_len(ncol(d_dfbetas))-1, "]") 

# Reshape to long format
d_dfbetas_long <- d_dfbetas %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id,
               names_to = "Variable",
               values_to = "value")  # Keep `id` intact

# Get influence threshold
cutoff <- svydfbetas(fit_svy)$cutoff

# Add influence criterion
d_dfbetas_long <- d_dfbetas_long %>%
  mutate(Criterion = ifelse(abs(value) > cutoff, "Yes", "No"))

# Ensure `id` is numeric
d_dfbetas_long$id <- as.numeric(d_dfbetas_long$id)

# Plot with β and subscripts in LaTeX format
ggplot(d_dfbetas_long %>% sample_n(min(20000, nrow(d_dfbetas_long))), aes(y = abs(value), x = id)) +
  geom_point(aes(color = Criterion)) +
  geom_hline(aes(yintercept = cutoff), linetype = "dashed") +
  facet_wrap(. ~ Variable, nrow = 4, labeller = label_parsed,
             scales = "free_y") +  # Ensure LaTeX formatting
  scale_color_manual(values = c("Yes" = "red", "No" = "black")) +
  labs(title = "DFBETAS Analysis",
       x = "Observation ID",
       y = expression(abs(beta))) +
  theme_minimal(base_size = 20) +
  theme(plot.title = element_text(hjust = 0.5))  # Centers the title

## DFFITS Analysis


# Create DFFITS dataframe
d_dffits <- data.frame(dffits = svydffits(fit_svy)$Dffits,
                       id = 1:length(svydffits(fit_svy)$Dffits))

# Get cutoff threshold for influence
cutoff <- svydffits(fit_svy)$cutoff

# Add influence criterion
d_dffits <- d_dffits %>%
  mutate(Criterion = ifelse(abs(dffits) > cutoff, "Yes", "No"))  # Translated "Si" to "Yes" and "No" remains the same

# Plot DFFITS with cutoff threshold
ggplot(d_dffits, aes(y = abs(dffits), x = id)) +
  geom_point(aes(color = Criterion)) +
  geom_hline(yintercept = cutoff,
             linetype = "dashed",
             color = "black") +  # Dashed horizontal line
  scale_color_manual(values = c("Yes" = "red", "No" = "black")) +
  labs(
    title = "DFFITS Influence Analysis",
    x = "Observation ID",
    y = expression(abs(DFFITS))
  ) +
  theme_minimal(base_size = 20) +
  theme(plot.title = element_text(hjust = 0.5))  # Centers the title

########################################
#  Inference on Model Parameters
########################################
library(broom)

# Extract coefficients and confidence intervals
coef_df <- tidy(fit_svy, conf.int = TRUE)
coef_df %>% kable(digits = 2)

ggplot(coef_df, aes(x = reorder(term, estimate), y = estimate)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2,
                color = "black") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(
    title = "Survey Model Coefficients",
    subtitle = "Point estimates with 95% confidence intervals",
    x = "Coefficients",
    y = "Estimate"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(hjust = 0.5),      # Center title
    plot.subtitle = element_text(hjust = 0.5)   # Center subtitle
  ) +
  coord_flip()  # Flip for better readability
