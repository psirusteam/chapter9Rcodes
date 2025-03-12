#########################################################
# Lectura y preparación de las bases de datos          
# Autor: Andrés Gutiérrez             
#########################################################

### Limpieza del entorno ###
rm(list = ls())
gc()

#################
### Librerías ###
#################
library(dplyr)
library(survey)
library(srvyr)
library(sae)
library(lme4)
library(data.table)
library(magrittr)
library(fastDummies)
library(haven)
library(stringr)
library(tidyr)
library(knitr)
library(kableExtra)
library(broom)
library(modelsummary)
library(gtsummary)
library(ggplot2)


# Asegurar el uso correcto de la función select de dplyr
select <- dplyr::select

###############################
### Carga de bases de datos ###
###############################

## Gasto per cápita
data_expenditure <- read_sav("data/bases/sect7b_hh_w4_v2.sav") %>%
  mutate(expenditure = ifelse(s7q03 == 2, 0, s7q04)) %>%
  group_by(household_id) %>%
  summarise(total_expenditure = sum(expenditure, na.rm = TRUE))

## Información general del hogar
data_sec <- read_sav("data/bases/sect1_hh_w4.sav")

## Resumen de variables clave
cat("Dimensiones de data_sec:", dim(data_sec), "\n")

# Cálculo de poblaciones según edad
cat("Población de 18 años o más:", sum(data_sec$s1q03a >= 18), "\n")
cat("Suma total de pesos muestrales:", sum(data_sec$pw_w4), "\n")
cat("Peso muestral para mayores de 18 años:", sum(data_sec$pw_w4[data_sec$s1q03a >= 18]), "\n")
cat("Peso muestral para menores de 18 años:", sum(data_sec$pw_w4[data_sec$s1q03a < 18]), "\n")

## Creación de variable de grupo etario y otras características demográficas
data_sec_age <- data_sec %>% 
  transmute(
    household_id,
    individual_id,
    ea_id,  # Identificador de unidades primarias de muestreo (EA)
    pw_w4,
    saq01, # Región
    saq14, # Área
    strata = paste0(saq01, "_", saq14), # Estratificación región-zona
    s1q03a, # Edad en años
    age_group = case_when(
      s1q03a < 18 ~ NA_character_,
      s1q03a < 31 ~ "18-30",
      s1q03a < 46 ~ "31-45", 
      s1q03a < 66 ~ "46-65", 
      TRUE ~ "66 o más"
    ),
    sexo = as_factor(s1q02),
    religion = as_factor(s1q08) %>% 
      recode(
        "6. PEGAN" = "OTHER", 
        "5. TRADITIONAL" = "OTHER",
        "8. OTHER (SPECIFY)" = "OTHER", 
        "7. WAKEFETA" = "OTHER"
      ) %>% 
      replace_na("OTHER")
  )

## Estado civil
data_sec4 <- read_dta("data/bases/sect1_hh_w4.dta")

data_sec4_MARRIED <- data_sec4 %>% 
  transmute(
    household_id,
    individual_id,
    s1q09 = as.character(as_factor(s1q09)) %>% replace_na("not answered")
  )

## Cobertura del hogar
data_sec3 <- read_sav("data/bases/sect_cover_hh_w4.sav") %>%
  inner_join(data_expenditure, by = "household_id")

data_sec_expenditure <- data_sec3 %>% 
  transmute(
    household_id,
    percapita_expenditure = total_expenditure / saq09
  )

###########################################
### Unión de bases para regresión final ###
###########################################

data_sec_regression <- data_sec_age %>%
  inner_join(data_sec4_MARRIED,
             by = c("household_id", "individual_id")) %>%
  inner_join(data_sec_expenditure, by = "household_id") %>%
  filter(s1q03a >= 18)

cat("Suma total de pesos muestrales en data_sec_regression:", 
    sum(data_sec_regression$pw_w4), "\n")

## Tablas de frecuencias para variables categóricas
data_sec_regression %>% 
  mutate(across(c(saq01, saq14), as_factor)) %>% 
  select(where(is.factor), where(is.character),
         -household_id, -individual_id, -ea_id) %>% 
  purrr::map(~table(.x, useNA = "always"))

## Conversión a data.table
data_sec_regression <- data_sec_regression %>% 
  mutate(across(c(saq01, saq14), as_factor)) %>% 
  data.table()

###############################################################################
# Pfeffermann
###############################################################################

modwk <-
  lm(pw_w4 ~  saq14 * religion + saq14 + sexo + age_group    ,
     data = data_sec_regression)

wkpred <- predict(modwk)
summary(wkpred)
data_sec_regression %<>% mutate(pw_qw = pw_w4 / wkpred)


summary(data_sec_regression$pw_qw)

plot(data_sec_regression$pw_w4, data_sec_regression$pw_qw)

par(mfrow=c(2, 2))

hist(data_sec_regression$pw_w4)
hist(data_sec_regression$pw_qw)
boxplot(data_sec_regression$pw_w4)
boxplot(data_sec_regression$pw_qw)

###############################################################################

# Linear models

design_sampleing <- data_sec_regression %>%
   filter(percapita_expenditure > 0)   %>% 
   as_survey_design(
    ids = ea_id,
    # Identificador de las unidades primarias de muestreo (EA)
    strata =  strata,
    # Estratificación por región (saq01) y zona urbana/rural (saq14)
    weights = pw_qw,
    # Peso final ajustado
    nest = TRUE
  ) %>% 
  mutate(log_expenditure = log(percapita_expenditure + 2) )

fit_svy <-
  svyglm(log_expenditure  ~ -1 + saq14 * religion + saq14 + sexo + age_group,
         design = design_sampleing)


summary(fit_svy)
tidy(fit_svy) %>% kable(digits = 2)

# modelsummary(
#  list(svyglm =  fit_svy),
#   fmt = 1,
#   estimate  = "{estimate} [{conf.low}, {conf.high}]",
#   statistic = "p.value",
#   coef_omit = "Intercept",
#   output = "markdown")

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
ggplot(data = data.frame(stdresids), aes(sample = stdresids)) +
  stat_qq() +
  stat_qq_line(color = "red") +  # Reference line in red
  labs(title = "Q-Q Plot of Standardized Residuals",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Centers the title

########################################
# Influential Observations
########################################

## Distancia de Cook

d_cook = data.frame(cook = svyCooksD(fit_svy),
                    id = 1:length(svyCooksD(fit_svy)))

ggplot(d_cook, aes(y = cook, x = id)) +
  geom_point() +
  theme_minimal(20)

# Calcular DFBETAS

d_dfbetas <- svydfbetas(fit_svy)$Dfbetas %>%
  t() %>%
  as.data.frame()

# Renombrar columnas de Beta
colnames(d_dfbetas) <- paste0("Beta_", seq_len(ncol(d_dfbetas)) - 1)

# Agregar ID y reorganizar en formato largo
d_dfbetas <- d_dfbetas %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = starts_with("Beta_"), names_to = "Variable", values_to = "value")

# Obtener umbral de influencia
cutoff <- svydfbetas(fit_svy)$cutoff

# Agregar criterio de influencia
d_dfbetas <- d_dfbetas %>%
  mutate(Criterio = ifelse(abs(value) > cutoff, "Si", "No"))

# Seleccionar las 10 observaciones más influyentes
tex_label <- d_dfbetas %>%
  filter(Criterio == "Si") %>%
  arrange(desc(abs(value))) 
 

# Mostrar las 10 observaciones más influyentes
tex_label %>%  slice_head(n = 20) %>%  kable(digits = 4)

ggplot(d_dfbetas %>% sample_n(10000), aes(y = abs(value), x = id)) +
  geom_point(aes(col = Criterio)) +
  geom_hline(aes(yintercept = cutoff)) +
  facet_wrap(. ~ Variable , nrow = 4) +
  scale_color_manual(values = c("Si" = "red", "No" = "black")) +
  theme_minimal(20)


## df fits

d_dffits <- data.frame(dffits = svydffits(fit_svy)$Dffits,
                       id = 1:length(svydffits(fit_svy)$Dffits))

cutoff <- svydffits(fit_svy)$cutoff

d_dffits %<>% mutate(C_cutoff = ifelse(abs(dffits) > cutoff, "Si", "No"))
ggplot(d_dffits, aes(y = abs(dffits), x = id)) +
  geom_point(aes(col = C_cutoff)) +
  geom_hline(yintercept = cutoff) +
  scale_color_manual(values = c("Si" = "red", "No" = "black")) +
  theme_minimal(20)

########################################
#  Inference on Model Parameters
########################################

library(jtools)
survey:::confint.svyglm(fit_svy) %>% kable(digits = 2)

plot_coefs(fit_svy,
           scale = TRUE,
           plot.distributions = TRUE)
summ(fit_svy, digits = 2,pvals = TRUE)


