#########################################################
# Lectura y preparación de las bases de datos           #
# Autor: Andrés Gutiérrez             #
#########################################################

### Cleaning R environment ###

rm(list = ls())
gc()

#################
### Libraries ###
#################

library(dplyr)
library(survey)
library(srvyr)
library(sae)
library(lme4)
library(data.table)
library(magrittr)
library(remotes)
library(fastDummies)
library(haven)
library(magrittr)
library(stringr)
library(tidyr)
select <- dplyr::select

library(knitr)
library(kableExtra)

###------------ Definiendo el límite de la memoria RAM a emplear ------------###

memory.limit(250000000)

################################################################################
###----------------------------- Loading datasets ---------------------------###
################################################################################
## Identificación del hogar; ubicación; tamaño del hogar e identificación
## del personal de campo 

dat_Hogar <- read_sav("data/bases/sect_cover_hh_w4.sav") %>% 
  select(household_id, # Unique Household Indentifier 
         ea_id , # Unique Enumeration Area Indentifier
         saq14,  # Rural/Urban
         saq02,  # saq02 Code
         saq01,  # saq01 code 
         pw_w4   # Final adjusted wave 4 weight
            )
dim(dat_Hogar)
# El ESS 2018/19 (ESS4) constituye un nuevo panel y no es una continuación de las 
# olas anteriores del ESS, sino una encuesta de referencia para las futuras rondas
# del ESS. Cubre los nueve estados saq01ales y dos ciudades administrativas, Addis
# Abeba y Dire Dawa. ESS4 se llevó a cabo en 565 Áreas de Enumeración (EAs), de
# las cuales 316 son rurales y 219 urbanas.

# La ESS4 tenía previsto entrevistar a 7.527 hogares de 565 áreas de enumeración (AE).
# La Tabla 1.1a muestra la distribución de las AE y los hogares de la muestra por
# región y estratos urbanos y rurales: se seleccionaron 316 AE de la AgSS rural y 
# 249 de la urbana. Se entrevistó a un total de 6.770 hogares de 535 AE tanto para 
# el módulo de agricultura como para el de hogares. Sin embargo, hay ocho AE
# adicionales y 124 hogares de áreas rurales que solo se sometieron a la encuesta
# durante la encuesta de agricultura (Tabla 1.1b).

n_distinct(dat_Hogar$ea_id)

dat_Hogar %>% distinct(ea_id,saq14 ) %>% group_by(saq14) %>% tally()

total_ea_id <- dat_Hogar %>% distinct(ea_id, saq14, saq01) %>% 
  mutate(saq14 = as_factor(saq14), 
         saq01 = as_factor(saq01)) %>% 
  group_by(saq01, saq14) %>% 
  tally() %>% 
  pivot_wider(names_from = saq14, values_from = n,  values_fill = list(n = 0)) %>% 
  mutate(TOTAL_ea_id = RURAL + URBAN) %>% 
  rename_with(~ paste0(., "_ea_id"), c("RURAL", "URBAN")) 

total_viv <- dat_Hogar %>% distinct(household_id, saq01,saq14 ) %>% 
  mutate(saq14 = as_factor(saq14), 
         saq01 = as_factor(saq01)) %>% 
  group_by(saq01,saq14) %>% 
  tally() %>% 
  pivot_wider(names_from = saq14, values_from = n,  values_fill = list(n = 0)) %>% 
  mutate(TOTAL_HH = RURAL + URBAN) %>% 
  rename_with(~ paste0(., "_HH"), c("RURAL", "URBAN")) 

paso <- inner_join(total_viv, total_ea_id) 

bind_rows(
  paso,
  paso %>% data.frame() %>%  summarise_if(is.numeric, sum) %>%
    mutate(saq01 = "Etiopia")
) %>% select(Region = saq01, 
             RURAL_HH, 
             RURAL_AE = RURAL_ea_id,
             URBAN_HH, 
             URBAN_AE =  URBAN_ea_id,
             TOTAL_HH,
             TOTAL_AE = TOTAL_ea_id) %>% 
  data.frame()

read_sav("data/bases/sect_cover_hh_w4.sav") %>% 
saveRDS("data/data_ESS4/sect_cover_hh_w4.rds")

read_sav("data/bases/sect13_hh_w4_v2.sav") %>% 
  saveRDS("data/data_ESS4/sect13_hh_w4_v2.rds")

################################################################################

data_sec <- read_sav("data/bases/sect7b_hh_w4_v2.sav")

data_sec %>% group_by(item_cd_12months,s7q03 ) %>% 
  tally() 


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

tab_03 <- design_sampleing %>% summarise(
    N_hat = survey_total(yes_no,na.rm = TRUE, vartype = c("se", "cv")),
    P_hat  = survey_mean(yes_no, na.rm = TRUE, vartype = c("se", "cv")),
    T_hat  = survey_total(expenditure, na.rm = TRUE, vartype = c("se", "cv")),
    M_hat  = survey_mean(expenditure, na.rm = TRUE, vartype = c("se", "cv"))
  )  

saveRDS(tab_03, "data/tablas/secc3/tab_03.rds")


readRDS("data/tablas/secc3/tab_03.rds") %>%
  transmute(
    item = as_factor(item),
    N_hat = round(N_hat),
    N_hat_se = round(N_hat_se),
    N_hat_cv = round(N_hat_cv*100, 2),
    P_hat = round(P_hat*100, 2),
    P_hat_cv = round(P_hat_cv*100, 2),
    P_hat_se = round(P_hat_se*100, 2)
) %>% 
  kable(digits = 2, caption = "Estimate by age group and place of residence, Ethiopia, 2018/2019")



readRDS("data/tablas/secc3/tab_03.rds") %>%
  transmute(
    item = as_factor(item),
    T_hat = round(T_hat),
    T_hat_cv = round(T_hat_cv*100, 2),
    T_hat_se = round(T_hat_se),
    M_hat = round(M_hat),
    M_hat_cv = round(M_hat_cv*100, 2),
    M_hat_se = round(M_hat_se, 2)
  ) %>% 
  kable(digits = 2, caption = "Estimate by age group and place of residence, Ethiopia, 2018/2019")

#######################################################################################
## Ejemplo de la mediana 
#######################################################################################

data_sec <- read_sav("data/bases/sect13_hh_w4_v2.sav")

data_sec <- data_sec %>% group_by(item = source_cd) %>%
  mutate(income = ifelse(s13q01  == 2, 0,s13q02 ),
         yes_no = ifelse(s13q01 == 2, 0,1 )
  ) 


design_sampleing <- data_sec %>% 
  mutate(strata = paste0(saq01, "_", saq14) ) %>% 
  as_survey_design(
    ids = ea_id,  # Identificador de las unidades primarias de muestreo (EA)
    strata =  strata, # Estratificación por región (saq01) y zona urbana/rural (saq14)
    weights = pw_w4,    # Peso final ajustado
    nest = TRUE
  )

# TABLE 5.3
# Other Income
design_sampleing %>% filter(s13q01 == 1) %>% 
  summarise(
    M_hat  = survey_mean(income, na.rm = TRUE, vartype = c("se", "cv")),
    Md_hat  = survey_median(income, na.rm = TRUE, vartype = c("se", "cv"))
  )


tab_04_P <- design_sampleing %>% summarise(
  P_hat  = survey_mean(yes_no, na.rm = TRUE, vartype = c("se", "cv"))
)  

tab_04_Md <- design_sampleing %>% filter(s13q01 == 1) %>% summarise(
   Md_hat  = survey_median(income, na.rm = TRUE, vartype = c("se", "cv"))
)  

tab_04 <- inner_join(tab_04_P, tab_04_Md)

saveRDS(tab_04, "data/tablas/secc3/tab_04.rds")


readRDS("data/tablas/secc3/tab_04.rds") %>%
  transmute(
    item = as_factor(item),
    P_hat = round(P_hat*100, 2),
    P_hat_cv = round(P_hat_cv*100, 2),
    P_hat_se = round(P_hat_se*100, 2),
    Md_hat = round(Md_hat),
    Md_hat_se = round(Md_hat_se),
    Md_hat_cv = round(Md_hat_cv*100, 2)
  ) %>% 
  kable(digits = 2, caption = "Estimate by age group and place of residence, Ethiopia, 2018/2019")


################################################################################
## Razon (1)
## gastos de algo (educa)/  Gasto total 
################################################################################
data_sec <- read_sav("data/bases/sect7b_hh_w4_v2.sav")
  
data_sec <-
  data_sec %>%
  mutate(expenditure = ifelse(s7q03 == 2, 0,s7q04 )) %>% 
  group_by(household_id) %>%
  mutate(total_expenditure = sum(expenditure, na.rm = TRUE))

summary(data_sec$expenditure)
sum(data_sec$expenditure == 0)

summary(data_sec$total_expenditure)
sum(data_sec$total_expenditure == 0)

design_sampleing <- data_sec %>% 
  mutate(strata = paste0(saq01, "_", saq14) ) %>% 
  as_survey_design(
    ids = ea_id,  # Identificador de las unidades primarias de muestreo (EA)
    strata =  strata, # Estratificación por región (saq01) y zona urbana/rural (saq14)
    weights = pw_w4,    # Peso final ajustado
    nest = TRUE
  )


tab_05_R1_total <- design_sampleing %>%
  group_by(item = item_cd_12months)  %>% 
  mutate(expenditure = ifelse(s7q03 == 2, 0,s7q04 )) %>% 
  summarise(R_hat = survey_ratio(
    expenditure,
    total_expenditure,
    na.rm = TRUE,
    vartype = c("se", "cv")
  )) %>% mutate(Region = "Ethiopia")


tab_05_R1_region <-
  design_sampleing %>% 
  group_by(item = item_cd_12months,  Region = as_factor(saq01)) %>%
  summarise(R_hat = survey_ratio(
    expenditure,
    total_expenditure,
    na.rm = TRUE,
    vartype = c("se", "cv")
  ))

tab_05_R1 <- bind_rows(tab_05_R1_total, tab_05_R1_region)

saveRDS(tab_05_R1, "data/tablas/secc3/tab_05_R1.rds")

paso1 <- readRDS("data/tablas/secc3/tab_05_R1.rds") %>%
  transmute(
    Region, 
    item = as_factor(item),
    R_hat = round(R_hat*100, 2),
    R_hat = paste0(R_hat," (", round(R_hat_cv*100, 2),")") 
  ) %>%
  pivot_wider(names_from = Region   , 
              values_from = R_hat, 
               values_fill = list(n = 0))
paso1 %>%   
kable(digits = 2, caption = "Estimate by age group and place of residence, Ethiopia, 2018/2019")


################################################################################
## Razon (2)
## gastos de algo (educa)/  Gasto total 
################################################################################
data_sec <- read_sav("data/bases/cons_agg_w4.sav")


summary(data_sec$educ_cons_ann)
summary(data_sec$total_cons_ann)


design_sampleing <- data_sec %>% 
  mutate(strata = paste0(saq01, "_", saq14) ) %>% 
  as_survey_design(
    ids = ea_id,  # Identificador de las unidades primarias de muestreo (EA)
    strata =  strata, # Estratificación por región (saq01) y zona urbana/rural (saq14)
    weights = pw_w4,    # Peso final ajustado
    nest = TRUE
  )

tab_05_R2_total <- design_sampleing %>% 
  summarise(R_hat = survey_ratio(
  educ_cons_ann,
  total_cons_ann,
  na.rm = TRUE,
  vartype = c("se", "cv")
)) %>% mutate(Region = "Ethiopia")


tab_05_R2_region <-
  design_sampleing %>% group_by(Region = as_factor(saq01)) %>% 
  summarise(R_hat = survey_ratio(
    educ_cons_ann,
    total_cons_ann,
    na.rm = TRUE,
    vartype = c("se", "cv")
  ))


tab_05_R2 <- bind_rows(tab_05_R2_region, tab_05_R2_total)

saveRDS(tab_05_R2, "data/tablas/secc3/tab_05_R2.rds")

readRDS("data/tablas/secc3/tab_05_R2.rds") %>%
  transmute(
    Region, 
    R_hat = round(R_hat*100, 2),
    R_hat_cv = round(R_hat_cv*100, 2) 
  )  %>%   
  kable(digits = 2, caption = "Estimate by age group and place of residence, Ethiopia, 2018/2019")

################################################################################
# The Gini coefficient
################################################################################
library(convey)
data_expenditure <- read_sav("data/bases/sect7b_hh_w4_v2.sav") %>%
  mutate(expenditure = ifelse(s7q03 == 2, 0, s7q04)) %>%
  group_by(household_id) %>%
  summarise(total_expenditure = sum(expenditure, na.rm = TRUE))

data_sec <- read_sav("data/bases/sect_cover_hh_w4.sav")

sum(data_sec$saq09)
data_sec <- data_sec %>% inner_join(data_expenditure)
data_sec <-
  data_sec %>% mutate(percapita_expenditure = total_expenditure / saq09)

summary(data_sec$percapita_expenditure)

design_sampleing <- data_sec %>% 
  mutate(strata = paste0(saq01, "_", saq14) ) %>% 
  as_survey_design(
    ids = ea_id,  # Identificador de las unidades primarias de muestreo (EA)
    strata =  strata, # Estratificación por región (saq01) y zona urbana/rural (saq14)
    weights = pw_w4,    # Peso final ajustado
    nest = TRUE
  )

design_gini <- convey_prep(design_sampleing)
tab_06_tot <-
  svygini(~ percapita_expenditure, design = design_gini) %>%
  as.data.frame() %>%
  mutate(Region = "Ethiopia") %>% 
  rename(SE = percapita_expenditure)

library(purrr)

tab_06_region <- map_df(unique(data_sec$saq01), 
    ~svygini( ~ percapita_expenditure, design = design_gini %>% 
           filter(saq01  == .x))  %>% as.data.frame() %>% 
      mutate(Region = as_factor(.x))) %>% 
  rename(SE = percapita_expenditure)

tab_06 <-  bind_rows(tab_06_region, tab_06_tot)
rownames(tab_06) <- NULL
saveRDS(tab_06, "data/tablas/secc3/tab_06_gini.rds")

readRDS("data/tablas/secc3/tab_06_gini.rds") %>%
  transmute(
    Region, 
    gini = round(gini*100, 2),
    SE = round(SE*100, 2),
  )  %>%   
  kable(digits = 2, caption = "Estimate by age group and place of residence, Ethiopia, 2018/2019")
# 
# ## Curva de lorenz
# 
# png("data/tablas/secc3/lorenz_curve/lorenz_curve_Ethiopia.jpeg",
#     width = 800, height = 600, units = "px")
# paso <- svylorenz(
#   formula = ~ percapita_expenditure,
#   design = design_gini,
#   quantiles = seq(0, 1, .1),
#   alpha = .01
# ) %>% as.data.frame() %>% 
#   tibble::rownames_to_column(var = "L")  %>%  
#   mutate(Region = "Ethiopia")
# dev.off()
# 
# 
# 
# map_df(unique(data_sec$saq01), 
#        ~ {
#          nombre_archivo <-
#            paste0("data/tablas/secc3/lorenz_curve/lorenz_curve_", .x, ".jpeg")
#          png(nombre_archivo, width = 800, height = 600, units = "px")
#          svylorenz(
#            formula = ~ percapita_expenditure ,
#            design = design_gini %>% filter(saq01  == .x),
#            quantiles = seq(0, 1, .1),
#            alpha = .01
#          ) %>% as.data.frame() %>% 
#     tibble::rownames_to_column(var = "L")  %>%  
#       mutate(Region = as_factor(.x))
#          dev.off()
#          }
#        )


################################################################################
# Cross-tabulations
################################################################################
data_sec <- read_sav("data/bases/sect1_hh_w4.sav")

data_sec %>% filter(s1q03a >= 10 ) %>% dim()

table(data_sec$s1q08)

design_sampleing <- data_sec %>%
  mutate(strata = paste0(saq01, "_", saq14)) %>%
  as_survey_design(
    ids = ea_id,
    # Identificador de las unidades primarias de muestreo (EA)
    strata =  strata,
    # Estratificación por región (saq01) y zona urbana/rural (saq14)
    weights = pw_w4,
    # Peso final ajustado
    nest = TRUE
  ) %>%
  mutate(
    sexo = as_factor(s1q02),
    religion = as_factor(s1q08),
    religion = case_when(
      religion %in% c("6. PEGAN", "5. TRADITIONAL",
                      "8. OTHER (SPECIFY)", "7. WAKEFETA") |
        is.na(religion) ~ "OTHER",
      TRUE ~ religion
    )
  ) %>%
  filter(s1q03a >= 10)


tab_2_2_region <- design_sampleing %>%
  group_by(Region = as_factor(saq01), religion) %>%
  summarise(P_hat = survey_mean(vartype = c("se", "cv")),
            T_hat = survey_total(vartype = c("se", "cv")))


tab_2_2_region %>%
  dplyr::select(-P_hat_se) %>%
  mutate(P_hat = round(P_hat * 100, 1)) %>% 
pivot_wider(
  names_from = religion ,
  values_from = P_hat,
   values_fill = list(P_hat = 0)
)


tab_2_2_tot <- design_sampleing %>%
  group_by(Region = "Ethiopia", religion) %>%
  summarise(P_hat = survey_mean(vartype = c("se", "cv")), 
            T_hat = survey_total(vartype = c("se", "cv"))) 


 tab_2_2_tot %>%
  dplyr::select(-P_hat_se) %>%
  mutate(P_hat = round(P_hat * 100, 1)) %>% 
  pivot_wider(
    names_from = religion ,
    values_from = P_hat,
    values_fill = list(P_hat = 0)
  )


tab_2_2_zone <- design_sampleing %>%
  group_by(Region = as_factor(saq14), religion) %>%
  summarise(P_hat = survey_mean(vartype = c("se", "cv")),
            T_hat = survey_total(vartype = c("se", "cv"))) 


tab_2_2_zone %>%
  dplyr::select(-P_hat_se) %>%
  mutate(P_hat = round(P_hat * 100, 1)) %>% 
  pivot_wider(
    names_from = religion ,
    values_from = P_hat,
     values_fill = list(P_hat = 0)
  )

bind_rows(tab_2_2_region, tab_2_2_zone,tab_2_2_tot ) %>% 
  dplyr::select(P_hat, religion) %>%
  mutate(P_hat = round(P_hat * 100, 1)) %>% 
  pivot_wider(
    names_from = religion ,
    values_from = P_hat,
     values_fill = list(P_hat = 0)
  ) %>% dplyr::select(
    Region, Orthodox =  "1. ORTHODOX",
    Catholic = "2. CATHOLIC",
    Protestant = "3. PROTESTANT",
    Muslim = "4. MUSLEM", 
    Other =  OTHER
  ) %>%  kable(digits = 2)



bind_rows(tab_2_2_region, tab_2_2_zone,tab_2_2_tot ) %>% 
  dplyr::select(T_hat, religion) %>%
  mutate(T_hat = round(T_hat)) %>% 
  pivot_wider(
    names_from = religion ,
    values_from = T_hat,
     values_fill = list(T_hat = 0)
  ) %>% dplyr::select(
    Region, Orthodox =  "1. ORTHODOX",
    Catholic = "2. CATHOLIC",
    Protestant = "3. PROTESTANT",
    Muslim = "4. MUSLEM", 
    Other =  OTHER
  ) %>%  kable(digits = 2)
sum(tab_2_2_tot$T_hat)


############################################################################
data_sec <- read_sav("data/bases/sect1_hh_w4.sav")

data_sec <- data_sec %>% 
  mutate(
    education_mother  = 
      case_when(
        s1q20 %in% c( 98, 99, 93 ) ~ "01 No education",  # INFORMAL, ADULT, ALTERNATIVE, NON-REGULAR, DON'T KNOW  
        s1q20 %in% c(96, 94 ) ~ "02 Primary",  # INFORMAL, ADULT, ALTERNATIVE, NON-REGULAR, DON'T KNOW  
        s1q20 >= 0 & s1q20 <= 8 ~ "02 Primary",  # KINDERGARDEN to 8TH GRADE COMPLETE
        s1q20 %in% c(9:13, 21:26, 95 ) ~ "03 Secondary",  # KINDERGARDEN to 8TH GRADE COMPLETE
        is.na(s1q20) ~NA_character_, 
        TRUE ~ "04 Above secondary"
      ),
    education_father  = 
      case_when(
        s1q16 %in% c(98, 99, 93) ~ "01 No education",  # INFORMAL, ADULT, ALTERNATIVE, NON-REGULAR, DON'T KNOW  
        s1q16 %in% c(96,94 ) ~ "02 Primary",  # INFORMAL, ADULT, ALTERNATIVE, NON-REGULAR, DON'T KNOW  
        s1q16 >= 0 & s1q16 <= 8 ~ "02 Primary",  # KINDERGARDEN to 8TH GRADE COMPLETE
        s1q16 %in% c(9:13, 21:26, 95 ) ~ "03 Secondary",  # KINDERGARDEN to 8TH GRADE COMPLETE
        is.na(s1q16) ~ NA_character_, 
        TRUE ~ "04 Above secondary"
      ),
    
    s1q16 = as_factor(s1q16),
    s1q20 = as_factor(s1q20),
    zone = as_factor(saq14))

data_sec %>%
group_by(education_mother, s1q20)  %>%
  tally() %>% arrange(education_mother) %>%
  data.frame()
# # 
# data_sec %>% 
#   group_by(education_father, s1q16)  %>% 
#   tally() %>% arrange(education_father) %>% 
#   data.frame()
# 
# data_sec %>% 
#   group_by(education_father, education_mother)  %>% 
#   tally() %>% arrange(education_father, education_mother) %>% 
#   data.frame()
# 
# data_sec  %>% dim()

design_sampleing <- data_sec %>%
  mutate(strata = paste0(saq01, "_", saq14)) %>%
  as_survey_design(
    ids = ea_id,
    # Identificador de las unidades primarias de muestreo (EA)
    strata =  strata,
    # Estratificación por región (saq01) y zona urbana/rural (saq14)
    weights = pw_w4,
    # Peso final ajustado
    nest = TRUE
  ) 

# TABLE 2.4
# Education and Occupation of Biological Parents
## Nacional padre 
 
tab_07_tot_father <- design_sampleing %>% 
  filter(s1q03a < 18, !is.na(education_father)) %>%
  group_by(Education = education_father) %>%
  summarise(P_father = survey_mean(vartype = c("se", "cv")))  %>%
  mutate(father = round(P_father * 100, 1))

## Nacional madre

tab_07_tot_mother <- design_sampleing %>%  filter(s1q03a < 18,
                             !is.na(education_mother)) %>%
  group_by(Education = education_mother) %>%
  summarise(P_mother = survey_mean(vartype = c("se", "cv"))) %>%
  mutate(mother = round(P_mother * 100, 1))

# Zona Padre 

tab_07_zone_father <- design_sampleing %>%  filter(s1q03a < 18,
                             !is.na(education_father)) %>%
  group_by( zone, Education = education_father) %>%
  summarise(P_father = survey_mean(vartype = c("se", "cv"))) 

tab_07_zone_father_P <- tab_07_zone_father %>%
  transmute(zone, Education      ,
            father = round(P_father * 100, 1)) %>%
  pivot_wider(
    names_from = zone ,
    values_from = father,
    values_fill = list(father = 0),
    names_prefix = "father "
  )

# Zona Madre

tab_07_zone_mother <- design_sampleing %>%
  filter(s1q03a < 18,
         !is.na(education_mother)) %>%
  group_by(zone, Education = education_mother) %>%
  summarise(P_mother = survey_mean(vartype = c("se", "cv")))

tab_07_zone_mother_P <- tab_07_zone_mother %>%
  transmute(zone, Education ,
    mother = round(P_mother * 100, 1))  %>%
  pivot_wider(
    names_from = zone ,
    values_from = mother,
    values_fill = list(mother = 0),
    names_prefix = "mother "
  )



tab_07 <- inner_join(tab_07_tot_mother, tab_07_tot_father) %>%
  inner_join(inner_join(tab_07_zone_father_P, tab_07_zone_mother_P )) %>%
  dplyr::select(Education, father,   mother,
                matches("RURAL"), matches("URBAN"))

tab_07 %>% kable(digits = 2)

## Tablas para independencia 

svytable( ~ education_mother + zone , 
          design_sampleing, Ntotal = 1) %>% 
  addmargins() %>% 
  kable()


svytable( ~ education_father + zone , 
          design_sampleing, Ntotal = 1) %>% 
  addmargins() %>% 
  kable()


svychisq(~ education_mother + zone , design_sampleing )
svychisq(~ education_father + zone , design_sampleing )


###########################################################
# Hypothesis Test for the Difference of Means

data_expenditure <- read_sav("data/bases/sect7b_hh_w4_v2.sav") %>%
  mutate(expenditure = ifelse(s7q03 == 2, 0, s7q04)) %>%
  group_by(household_id) %>%
  summarise(total_expenditure = sum(expenditure, na.rm = TRUE))

data_sec <- read_sav("data/bases/sect_cover_hh_w4.sav")

sum(data_sec$saq09)
data_sec <- data_sec %>% inner_join(data_expenditure)
data_sec <-
  data_sec %>%
  mutate(percapita_expenditure = total_expenditure / saq09)

summary(data_sec$percapita_expenditure)

design_sampleing <- data_sec %>% 
  mutate(strata = paste0(saq01, "_", saq14) ) %>% 
  as_survey_design(
    ids = ea_id,  # Identificador de las unidades primarias de muestreo (EA)
    strata =  strata, # Estratificación por región (saq01) y zona urbana/rural (saq14)
    weights = pw_w4,    # Peso final ajustado
    nest = TRUE
  )

svyttest(percapita_expenditure ~ saq14, 
         design = design_sampleing, level = 0.95) 

svyttest(percapita_expenditure ~ saq14, 
         design = design_sampleing %>% filter(saq01 == 1), 
         level = 0.95) 

## Linear models
