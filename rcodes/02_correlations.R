# educ_cons_ann
# Gasto anual en educación (incluida la asistencia)
# Valid: 6770

# utilidades_cons_ann
# Gasto anual en servicios públicos
# Valido: 6770

# fafh_cons_ann
# Gasto anual en comida fuera de casa
# Valido: 6770
dat_pers <- read_sav("data/bases/cons_agg_w4.sav") %>% 
  mutate(strata = paste0(saq01, "_", saq14) )

design_sampleing <- dat_pers %>% # Base de datos
  as_survey_design(
    ids = ea_id,  # Identificador de las unidades primarias de muestreo (EA)
    strata =  strata, # Estratificación por región (saq01) y zona urbana/rural (saq14)
    weights = pw_w4,    # Peso final ajustado
    nest = TRUE
  )

summary(dat_pers$educ_cons_ann)
summary(dat_pers$utilities_cons_ann)
summary(dat_pers$fafh_cons_ann)

sum(dat_pers$educ_cons_ann>0)
sum(dat_pers$utilities_cons_ann>0)
sum(dat_pers$fafh_cons_ann>0)

par(mfrow = c(1,2))
plot(dat_pers$educ_cons_ann,dat_pers$utilities_cons_ann )
plot(dat_pers$educ_cons_ann,dat_pers$fafh_cons_ann )

design_sampleing %>%
  summarise(corr1 = survey_corr(educ_cons_ann,utilities_cons_ann), 
            corr2 = survey_corr(educ_cons_ann,fafh_cons_ann))

design_sampleing %>% group_by(saq14) %>%
  summarise(corr1 = survey_corr(educ_cons_ann,utilities_cons_ann), 
            corr2 = survey_corr(educ_cons_ann,fafh_cons_ann))

design_sampleing %>% group_by(saq01) %>%
  summarise(corr1 = survey_corr(educ_cons_ann,utilities_cons_ann), 
            corr2 = survey_corr(educ_cons_ann,fafh_cons_ann))



