#########################################################
# Lectura y preparación de las bases de datos          
# Autor: Andrés Gutiérrez  
# Data visualization
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
library(scales)


# Bar charts

options(survey.lonely.psu = "adjust") # Tiene error cuando uso adjust
data_sec <- read_sav("data/bases/sect7b_hh_w4_v2.sav")

data_sec <- data_sec  %>%
  group_by(item = item_cd_12months) %>%
  mutate(expenditure = ifelse(s7q03 == 2, 0,s7q04 ),
         yes_no = ifelse(s7q03 == 2, 0,1 )
  ) 

design_sampleing <- data_sec %>% 
  mutate(strata = paste0(saq01, "_", saq14) ) %>% 
  as_survey_design(
    ids = ea_id,  # Identificador de las unidades primarias de muestreo (EA)
    strata =  strata, # Estratificación por región (saq01) y zona urbana/rural (saq14)
    weights = pw_w4,    # Peso final ajustado
    nest = TRUE
  )

#TABLE 7.3
# Spending on Nonfood Items and Services

tab_03 <- design_sampleing   %>% summarise(
  N_hat = survey_total(yes_no, na.rm = TRUE, vartype = c("se", "ci")),
  T_hat  = survey_total(expenditure, na.rm = TRUE, vartype = c("se", "ci"))
) %>%
  mutate(item = as_factor(item))


ggplot(data = tab_03,
       aes(
         x = item,
         y = N_hat,
         ymax = N_hat_upp,
         ymin = N_hat_low,
         fill = item
       )) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(position = position_dodge(width = 0.9), width = 0.3) +
  scale_fill_manual(values = colorRampPalette(c("#CCE5FF", "#0057B7"))(length(unique(tab_03$item)))) +  # Escala de azules
  scale_x_discrete(expand = c(0.1, 0.1)) +  # Evita cortes en los nombres del eje X
  scale_y_continuous(labels = label_number(accuracy = 1)) +  # Mantiene todos los dígitos sin notación científica
  labs(y = expression(hat(N)), x = "") +  # Formato LaTeX para N_hat
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Rota etiquetas largas del eje X
        legend.title = element_blank()  # Oculta título de la leyenda
        )
        

ggplot(data = tab_03,
       aes(
         x = item,
         y = T_hat,
         ymax = T_hat_upp,
         ymin = T_hat_low,
         fill = item
       )) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(position = position_dodge(width = 0.9), width = 0.3) +
  scale_fill_manual(values = colorRampPalette(c("#D9D9D9", "#4D4D4D"))(length(unique(tab_03$item)))) +  # Escala de grises
  scale_x_discrete(expand = c(0.1, 0.1)) +  # Evita cortes en los nombres del eje X
  scale_y_continuous(labels = label_number(accuracy = 1)) +  # Mantiene todos los dígitos sin notación científica
  labs(y = expression(hat(T)), x = "") +  # Formato LaTeX para N_hat
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Rota etiquetas largas del eje X
        legend.title = element_blank()  # Oculta título de la leyenda
        )
############################################# 
# 7.2 Histograms       
#############################################
## Gasto per cápita
data_expenditure <- read_sav("data/bases/sect7b_hh_w4_v2.sav") %>%
  mutate(expenditure = ifelse(s7q03 == 2, 0, s7q04)) %>%
  group_by(household_id) %>%
  summarise(total_expenditure = sum(expenditure, na.rm = TRUE))

data_sec <- read_sav("data/bases/sect_cover_hh_w4.sav")

data_sec <- data_sec %>% inner_join(data_expenditure)
data_sec <-
  data_sec %>%
  mutate(percapita_expenditure = total_expenditure / saq09, 
         log_percapita_expenditure = log(percapita_expenditure+2))


design_sampleing <- data_sec %>% 
  mutate(strata = paste0(saq01, "_", saq14) ) %>% 
  as_survey_design(
    ids = ea_id,  # Identificador de las unidades primarias de muestreo (EA)
    strata =  strata, # Estratificación por región (saq01) y zona urbana/rural (saq14)
    weights = pw_w4,    # Peso final ajustado
    nest = TRUE
  )
x11()
svyhist(
  ~ log_percapita_expenditure,
  design_sampleing,
  main = " Population expenditure",
  col = "grey80",breaks = 30,
  xlab = "Log-Expenditure",
  probability = FALSE
)

# Gráfico con ggplot2
ggplot(data_sec, aes(x = log_percapita_expenditure, weight = pw_w4)) +
  geom_histogram(fill = "grey80",
                 color = "black",
                 bins = 30) +
  labs(title = "Population Expenditure",
       x = "Log-Expenditure",
       y = "Count") +  scale_y_continuous(labels = label_number(accuracy = 1)) +
  theme_minimal()


# Gráfico con ggplot2
ggplot(
  data_sec %>% mutate(saq14 = as_factor(saq14)),
  aes(x = log_percapita_expenditure, weight = pw_w4)
) +
  geom_histogram(fill = "grey80",
                 color = "black",
                 bins = 30) +
  labs(title = "Population Expenditure",
       x = "Log-Expenditure",
       y = "Count") +  scale_y_continuous(labels = label_number(accuracy = 1)) +
  facet_grid(. ~ saq14) +
  theme_minimal()

############################################# 
# 7.3  Scatter Plots       
#############################################
data_sec <- read_sav("data/bases/sect13_hh_w4_v2.sav")

data_sec <- data_sec %>% group_by(item = source_cd) %>%
  mutate(income = ifelse(s13q01  == 2, 0,s13q02 ),
         yes_no = ifelse(s13q01 == 2, 0,1 )
  ) 

data_income <- data_sec %>%  filter(s13q01 == 1) %>% 
  group_by(household_id) %>% 
  summarise(total_income = sum(income))

data_income <- inner_join(data_expenditure, data_income) 

data_sec <- read_sav("data/bases/sect_cover_hh_w4.sav")

data_sec <- data_sec %>% inner_join(data_income)
data_sec <-
  data_sec %>%
  mutate(percapita_expenditure = total_expenditure / saq09, 
         log_percapita_expenditure = log(percapita_expenditure+2),
         percapita_income = total_income / saq09, 
         log_percapita_income = log(percapita_income + 2))

 
# Gráfico con ggplot2
ggplot(
  data_sec,
  aes(x = log_percapita_expenditure, 
      y = log_percapita_income, weight = pw_w4)
) +  
  geom_point(aes(size = pw_w4), alpha = 0.1) +
  labs(
    title = "Relationship Between Log-Expenditure and Log-Income",
    subtitle = "Point size represents survey weights",
    x = "Log-Per Capita Expenditure",
    y = "Log-Per Capita Income"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(
  data_sec,
  aes(x = log_percapita_expenditure, 
      y = log_percapita_income, weight = pw_w4)
) +  
  geom_point(aes(color = pw_w4), alpha = 0.2) +
  labs(
    title = "Income vs Expenditure with Weight Coloring",
    subtitle = "Point color represents survey weights",
    x = "Log-Per Capita Expenditure",
    y = "Log-Per Capita Income",
    color = "Survey Weights"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(
  data_sec %>% mutate(saq14 = as_factor(saq14)),
  aes(x = log_percapita_expenditure, 
      y = log_percapita_income, weight = pw_w4)
) + 
  geom_point(aes(size = pw_w4, color = saq14), alpha = 0.3) +
  labs(
    title = "Income vs Expenditure by Area",
    subtitle = "Color represents urban or rural area",
    x = "Log-Per Capita Expenditure",
    y = "Log-Per Capita Income",
    color = "Area Type"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

