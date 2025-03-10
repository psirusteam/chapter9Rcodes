---
editor_options: 
  markdown: 
    wrap: 80
---

# Descriptive Parameters

In this subsection a NSO will share how they do disseminate its results on basic
descriptive statistics, how they publish the resulting tables and how do they
deal with the suppression of estimates that do not reach expected quality.

## Ethiopia Socioeconomic Survey (ESS4): Sampling Design and Data Source

The Ethiopia Socioeconomic Survey (ESS4) is part of the Living Standards Measurement Study (LSMS), an initiative by the World Bank in collaboration with the Central Statistical Agency of Ethiopia (CSA). Its objective is to provide high-quality data for the analysis of poverty, food security, agricultural development, and other key socioeconomic aspects.

The ESS4 is part of a series of longitudinal surveys initiated in 2011 under the name Ethiopia Rural Socioeconomic Survey (ERSS). In its first two rounds, the survey only covered rural areas and small towns, but starting from the second wave (ESS2, 2013/2014), the coverage was expanded to all urban areas, ensuring nationally representative estimates. The ESS1, ESS2, and ESS3 waves constitute Panel I, while ESS4 initiates a new panel, serving as the baseline for future surveys.

The survey provides information on household structure, employment, income, consumption, education, health, access to infrastructure, and characteristics of the agricultural sector. One of its key features is the combination of household survey data with administrative records and remote sensing technologies, such as satellite imagery, to improve the accuracy of economic and social indicator estimates.

### Data Source

The ESS4 was implemented by the CSA of Ethiopia, in collaboration with the World Bank and other international organizations. Data collection was conducted through direct interviews with selected households, using electronic devices through Computer-Assisted Personal Interviewing (CAPI). The database includes detailed information on:

-   Household structure and sociodemographic characteristics (age, gender, education, health).

-   Living conditions and economic well-being (income, consumption, access to services).

-   Housing characteristics (construction materials, access to drinking water, sanitation, and electricity).

-   Food security and agricultural production (crops, livestock, access to markets).

-   Access to financing and land tenure.

-   Mobility and migration (internal displacement and job seeking).

The ESS4 not only allows for the generation of descriptive statistics on the Ethiopian population but also facilitates longitudinal studies to assess changes in living conditions over time.

### Sampling Design

The ESS4 follows a probabilistic, stratified, and two-stage sampling design, ensuring representative coverage at the national and regional levels.

#### Sampling Frame

The sample selection was based on the updated cartography from the 2018 Population and Housing Census, ensuring an updated and accurate sampling frame.

-   *First Stage*: Selection of Enumeration Areas (EAs)

    In rural areas, Enumeration Areas (EAs) were selected from the sample of the Annual Agricultural Sample Survey (AgSS) using simple random sampling. In urban areas, the selection was carried out through systematic sampling with probability proportional to size (PPS).

    A total of 565 EAs were selected, of which 316 were rural and 249 were urban.

-   *Second Stage*: Selection of Households

    Within each selected EA, households were sampled using systematic random sampling:

    Rural areas: 10 agricultural households and 2 non-agricultural households were selected per EA.

    Urban areas: 15 households were selected per EA, regardless of their economic activity.

The final planned sample comprised 7,527 households, although due to security constraints, only 6,770 households in 535 EAs could be interviewed.


| Region            | RURAL Households  | RURAL AEs | URBAN Households  | URBAN AEs | N    | hat_N AEs |
|------------|------------|------------|------------|------------|------------|------------|
| TIGRAY            | 393               | 35        | 283               | 19        | 676      | 54        |
| AFAR              | 299               | 28        | 225               | 15        | 524      | 43        |
| AMHARA            | 479               | 43        | 271               | 18        | 750      | 61        |
| OROMIA            | 453               | 43        | 300               | 20        | 753      | 63        |
| SOMALI            | 355               | 35        | 255               | 17        | 610      | 52        |
| BENISHANGUL GUMUZ | 169               | 16        | 195               | 13        | 364      | 29        |
| SNNP              | 422               | 40        | 269               | 18        | 691      | 58        |
| GAMBELA           | 195               | 19        | 300               | 20        | 495      | 39        |
| HARAR             | 190               | 18        | 360               | 24        | 550      | 42        |
| ADDIS ABABA       | 0                 | 0         | 778               | 52        | 778      | 52        |
| DIRE DAWA         | 160               | 14        | 419               | 28        | 579      | 42        |
| **Etiopia**       | **3115**          | **291**   | **3655**          | **244**   | **6770** | **535**   |
Table: Resumen de la Muestra por Region

Description of the table:

- **RURAL Households**: Number of rural households surveyed in each region.

- **RURAL AEs**: Number of rural enumeration areas (AEs) in each region.

- **URBAN Households**: Number of urban households surveyed in each region.

- **URBAN AEs**: Number of urban enumeration areas (AEs) in each region.

- **N Households**: Total number of households surveyed in each region (rural + urban).

- **N AEs**: Total number of enumeration areas (AEs) in each region (rural + urban).


## Definición del diseño muestral en R 

First, the necessary libraries are loaded: **`haven`** to read data files in `.sav` format (SPSS), **`dplyr`** for data manipulation, **`survey`** for the analysis of complex surveys, and **`srvyr`**, which is an extension of `survey` with a syntax similar to `dplyr`.


``` r
library(haven)       # Para leer archivos .sav
library(dplyr)       # Para manipulación de datos
library(survey)      # Para análisis de encuestas complejas
library(srvyr)
library(magrittr)
```


Next, the data is loaded from an `.rds` file using the **`readRDS`** function. The data is stored in the object **`dat_Hogar`**. Then, the relevant columns for the analysis are selected using the **`transmute`** function. These columns include: **`household_id`** (unique household identifier), **`ea_id`** (unique identifier for the enumeration area, representing the primary sampling units), **`saq14`** (variable indicating whether the household is rural (1) or urban (2)), **`saq02`** (region or district code), **`saq01`** (region code), and **`pw_w4`** (final adjusted weight for the fourth wave of the survey, necessary for weighted analysis).



``` r
dat_Hogar <- readRDS("data/data_ESS4/sect1_hh_w4.rds") %>% 
  mutate(strata = paste0(saq01, "_", saq14) )
```


Finally, the complex survey design is defined using the **`as_survey_design`** function from the `srvyr` package. This design considers: **`ids = ~ ea_id`** to specify the primary sampling units (enumeration areas), **`strata = ~ saq01 + saq14`** to define stratification by region and urban/rural area, **`weights = ~ pw_w4`** to assign sampling weights, and **`nest = TRUE`** to indicate that the cluster identifiers are nested within the strata. This survey design is essential for performing statistical analyses that take into account the complex structure of the survey, such as estimating means, hat_Ns, or regressions.


``` r
# Definir el diseño muestral
design_sampleing <- dat_Hogar %>% # Base de datos
  as_survey_design(
    ids = ea_id,  # Identificador de las unidades primarias de muestreo (EA)
    strata =  strata, # Estratificación por región (saq01) y zona urbana/rural (saq14)
    weights = pw_w4,    # Peso final ajustado
    nest = TRUE
  )
```

## Frequencies and proportions

In the following section, we illustrate the estimation process using `R` routines, specifically for computing hat_Ns and proportions. We begin by describing the routine for estimating the hat_N number of people by region.

When some strata contain only one cluster (PSU), variance estimation errors may occur. To address this, you can use the following option:  
`options(survey.lonely.psu = "adjust")`  
This adjustment ensures that the variance is computed for strata with a single cluster rather than discarding them, leading to more robust variance estimation.

The process starts by converting the `Region` variable into a factor using `as_factor(saq01)`, ensuring it is treated as a categorical variable. Next, the data is grouped by `Region` using `group_by(Region)`. Within each group, the hat_N estimate of the variable of interest is calculated using the `survey_hat_N()` function. This function provides estimates of the population hat_N along with key precision metrics, including the coefficient of variation (`cv`), standard error (`se`), and confidence intervals (`ci`).



``` r
options(survey.lonely.psu = "adjust") # Tiene error cuando uso adjust
design_sampleing <-
  design_sampleing  %>%    mutate(Region  = as_factor(saq01))

tab_01 <- design_sampleing %>% group_by(Region) %>%
  summarise(N_hat = survey_total(vartype = c("cv", "se", "ci")))
```

After calculating these values, transformations are applied to improve their presentation:  

- `hat_N` is rounded to the nearest integer.  

- `hat_N_cv` is multiplied by 100 and rounded to two decimal places to express it as a percentage.  

- `hat_N_se`, `hat_N_low` (lower limit of the confidence interval), and `hat_N_upp` (upper limit of the confidence interval) are rounded to integer values.  

Finally, the resulting table (`tab_01`) is saved to the file `"data/tables/secc3/tab_01.rds"` using `saveRDS()`



``` r
tab_01 <- tab_01 %>% mutate(
  hat_N = round(hat_N),
  hat_N_cv = round(hat_N_cv * 100 , 2),
  hat_N_se = round(hat_N_se),
  hat_N_low = round(hat_N_low),
  hat_N_upp = round(hat_N_upp)
)

saveRDS(tab_01, "data/tablas/secc3/tab_01.rds")
```




The results present population estimates for different regions along with their respective coefficients of variation (`CV`), standard errors (`SE`), and confidence intervals (`CI`). In terms of data quality, larger regions, such as Oromia and Amhara, exhibit lower coefficients of variation (3.23% and 2.48%, respectively), indicating greater precision in their estimates. On the other hand, smaller regions, such as Afar (10.03%) and Benishangul Gumuz (6.79%), show relatively high `CV` values, suggesting greater uncertainty in their estimates.  

The standard error (`SE`) is higher in regions with larger populations, such as Oromia (1,151,677) and Amhara (531,161), which is expected since absolute variability increases with population size. However, when considering the `CV`, it remains within acceptable levels for most regions.  

Confidence intervals (`CI`) help assess the reliability of the estimates. In general, regions with higher `CV` values have wider confidence intervals, reflecting greater uncertainty. For instance, Afar has a confidence interval of approximately ±169,061 people, whereas in Amhara, the range is relatively narrower at ±1,043,026 people. This suggests that estimates in smaller regions may be less precise and could benefit from increased sample coverage or refinements in the sampling design.

Table: Estimated population by place of residence, Ethiopia, 2018/2019

|        Region        |  hat_N   | hat_N_cv | hat_N_se | hat_N_low | hat_N_upp |
|----------------------|----------|----------|----------|-----------|-----------|
|      1. TIGRAY       | 5738683  |   3.34   |  191814  |  5361848  |  6115519  |
|       2. AFAR        |  863300  |  10.03   |  86563   |  693239   |  1033360  |
|      3. AMHARA       | 21438509 |   2.48   |  531161  | 20394996  | 22482022  |
|      4. OROMIA       | 35642718 |   3.23   | 1151677  | 33380144  | 37905291  |
|      5. SOMALI       | 5094817  |   4.90   |  249802  |  4604059  |  5585575  |
| 6. BENISHANGUL GUMUZ |  911063  |   6.79   |  61876   |  789502   |  1032625  |
|       7. SNNP        | 19384292 |   2.85   |  552278  | 18299293  | 20469291  |
|     12. GAMBELA      |  389684  |   4.58   |  17862   |  354592   |  424776   |
|      13. HARAR       |  254362  |   4.05   |  10309   |  234110   |  274614   |
|   14. ADDIS ABABA    | 3787428  |   2.86   |  108157  |  3574943  |  3999912  |
|    15. DIRE DAWA     |  409426  |   4.69   |  19196   |  371714   |  447139   |


### Demographic Characteristics

The code begins by transforming the `design_sampleing` dataset using the `mutate` function, creating new variables derived from age (`s1q03a`). Three age group classifications are defined: `Age_Group0_5` (children under 6 and the rest), `Age_Group0_9` (children under 10 and the rest), and `Age_Group`, which categorizes ages into three groups: `0 - 14`, `15 - 64`, and `65 +`.



``` r
design_sampleing %<>% 
  mutate(
     Age_Group0_5 = case_when(s1q03a < 6 ~ "0 - 5",
                           TRUE ~ "6 +"), 
     Age_Group0_9 = case_when(s1q03a < 10 ~ "0 - 9",
                           TRUE ~ "9 +"),
     Age_Group = case_when(s1q03a < 15 ~ "0 - 14",
                           s1q03a < 65 ~ "15 - 64",
                               TRUE ~ "65 +"))
```


Next, three tables (`tab2.1_0_5`, `tab2.1_0_9`, and `tab2.1_0_14`) are generated to summarize the population proportion for each age group by region. Each table groups the data by `Region` and the corresponding age variable, calculating the sample mean of the variable of interest using the `survey_mean()` function. Additionally, the proportion values are expressed as percentages and rounded to one decimal place. For the `0 - 5` and `0 - 9` age group tables, only the rows corresponding to these age groups are retained.



``` r
tab2.1_0_5 <- design_sampleing %>%
  group_by(Region  , Age_Group = Age_Group0_5) %>%
  summarise(prop = survey_mean(
    vartype = c("cv", "se", "ci"),
    proportion = TRUE,
    prop_method = "logit"
  )) %>%
  mutate(prop = round(prop * 100, 1)) %>%
  filter(Age_Group == "0 - 5")

tab2.1_0_9 <- design_sampleing %>%
  group_by(Region  , Age_Group = Age_Group0_9) %>%
  summarise(prop = survey_mean(
    vartype = c("cv", "se", "ci"),
    proportion = TRUE,
    prop_method = "logit"
  )) %>%
  mutate(prop = round(prop * 100, 1)) %>%
  filter(Age_Group == "0 - 9")

tab2.1_0_14 <- design_sampleing %>%
  group_by(Region  , Age_Group) %>%
  summarise(prop = survey_mean(
    vartype = c("cv", "se", "ci"),
    proportion = TRUE,
    prop_method = "logit"
  )) %>%
  mutate(prop = round(prop * 100, 1))
```

These operations are then replicated in an additional dataset where the `Region` variable is set to `"Ethiopia"` instead of taking the information from the original dataset. This results in three new tables (`tab2.1_0_5t`, `tab2.1_0_9t`, and `tab2.1_0_14t`), processed in the same way as the previous ones but applied exclusively to Ethiopia.


``` r
tab2.1_0_5t <- design_sampleing %>%
  group_by(Region = "Ethiopia", Age_Group = Age_Group0_5) %>%
  summarise(prop = survey_mean(
    vartype = c("cv", "se", "ci"),
    proportion = TRUE,
    prop_method = "logit"
  )) %>%
  mutate(prop = round(prop * 100, 1)) %>%
  filter(Age_Group == "0 - 5")

tab2.1_0_9t <- design_sampleing %>%
  group_by(Region = "Ethiopia", Age_Group = Age_Group0_9) %>%
  summarise(prop = survey_mean(
    vartype = c("cv", "se", "ci"),
    proportion = TRUE,
    prop_method = "logit"
  )) %>%
  mutate(prop = round(prop * 100, 1)) %>%
  filter(Age_Group == "0 - 9")

tab2.1_0_14t <- design_sampleing %>%
  group_by(Region = "Ethiopia", Age_Group) %>%
  summarise(prop = survey_mean(
    vartype = c("cv", "se", "ci"),
    proportion = TRUE,
    prop_method = "logit"
  )) %>%
  mutate(prop = round(prop * 100, 1))
```

Finally, all the generated tables are combined into a single table, `tab_02`, using `bind_rows()`, consolidating the results for all regions and Ethiopia into one data structure. The final table is saved as an `.rds` file in `"data/tablas/secc3/tab_02.rds"`.



``` r
tab_02 <- bind_rows(tab2.1_0_14, tab2.1_0_9, tab2.1_0_5, 
                    tab2.1_0_14t, tab2.1_0_9t, tab2.1_0_5t) 
saveRDS(tab_02, "data/tablas/secc3/tab_02.rds")
```






The tables present population estimates by age group and place of residence in Ethiopia for the 2018/2019 period, along with their respective standard deviations.  

Overall, the proportion of the population in younger age groups (0 - 5, 0 - 9, and 0 - 14) is higher in regions such as Afar (46.4% in the 0 - 14 age group) and Somali (52.4% in the same group), indicating a younger population structure in these areas. In contrast, Addis Ababa has the lowest proportion of the population in the 0 - 14 age group (25.3%) and the highest in the 15 - 64 age group (70.4%), suggesting a more adult and urban demographic distribution.

|        Region        | 0 - 5 | 0 - 9 | 0 - 14 | 15 - 64 | 65 + |
|----------------------|-------|-------|--------|---------|------|
|      1. TIGRAY       | 15.8  | 26.0  |  39.4  |  55.8   | 4.8  |
|       2. AFAR        | 20.3  | 34.0  |  46.4  |  51.6   | 2.0  |
|      3. AMHARA       | 14.2  | 24.7  |  37.4  |  58.4   | 4.2  |
|      4. OROMIA       | 16.8  | 30.0  |  44.4  |  52.4   | 3.2  |
|      5. SOMALI       | 17.0  | 34.0  |  52.4  |  45.4   | 2.2  |
| 6. BENISHANGUL GUMUZ | 14.7  | 27.0  |  39.8  |  56.1   | 4.2  |
|       7. SNNP        | 15.1  | 29.5  |  45.4  |  52.0   | 2.6  |
|     12. GAMBELA      | 15.9  | 27.5  |  41.6  |  56.6   | 1.7  |
|      13. HARAR       | 16.0  | 28.4  |  40.8  |  55.4   | 3.8  |
|   14. ADDIS ABABA    | 11.9  | 17.6  |  25.3  |  70.4   | 4.3  |
|    15. DIRE DAWA     | 14.9  | 25.5  |  37.2  |  59.8   | 3.0  |
|       Ethiopia       | 15.6  | 28.1  |  42.3  |  54.3   | 3.4  |

Table: Estimate by age group and place of residence, Ethiopia, 2018/2019



The quality of the estimates can be assessed by looking at the standard deviations. In general, the values are relatively low, indicating stable estimates. However, some regions exhibit higher standard deviations in certain age groups, reflecting greater uncertainty in the estimation. For example, Benishangul Gumuz has a standard deviation of 2.93 for the 0 - 14 age group, suggesting relatively high variability in this category. Similarly, Afar shows a standard deviation of 2.03 in the 0 - 14 age group, indicating that the estimate for this region may be less precise.  

More urbanized regions, such as Addis Ababa, exhibit lower standard deviations across almost all age groups (e.g., 0.86 in the 0 - 14 age group and 0.74 in the 15 - 64 age group), indicating greater precision in the estimates for these areas. In contrast, regions with more dispersed and rural population structures show higher variations in estimates, reflecting greater uncertainty in data quality.

|        Region        | 0 - 5 | 0 - 9 | 0 - 14 | 15 - 64 | 65 + |
|----------------------|-------|-------|--------|---------|------|
|      1. TIGRAY       | 0.83  | 0.86  |  0.99  |  0.92   | 0.46 |
|       2. AFAR        | 1.33  | 1.74  |  2.03  |  2.06   | 0.38 |
|      3. AMHARA       | 0.84  | 1.12  |  1.12  |  1.09   | 0.45 |
|      4. OROMIA       | 0.92  | 1.13  |  1.02  |  0.90   | 0.45 |
|      5. SOMALI       | 0.82  | 1.18  |  1.23  |  1.22   | 0.37 |
| 6. BENISHANGUL GUMUZ | 1.75  | 2.69  |  2.93  |  2.56   | 0.80 |
|       7. SNNP        | 0.91  | 1.35  |  1.37  |  1.25   | 0.40 |
|     12. GAMBELA      | 0.89  | 1.24  |  1.75  |  1.94   | 0.39 |
|      13. HARAR       | 1.03  | 1.46  |  1.60  |  1.49   | 0.48 |
|   14. ADDIS ABABA    | 0.69  | 0.90  |  0.86  |  0.74   | 0.53 |
|    15. DIRE DAWA     | 1.13  | 1.54  |  1.88  |  1.87   | 0.37 |
|       Ethiopia       | 0.45  | 0.58  |  0.57  |  0.52   | 0.22 |

Table: Estimated standard deviation by age group by place of residence, Ethiopia, 2018/2019


### Totals, means and ratios

sect7b_hh_w4_v2.sav 


``` r
tab7_2 <- design_sampleing  %>%
  group_by(item = item_cd_12months) %>%
  mutate(expenditure = ifelse(s7q03 == 2, 0, s7q04),
         yes_no = ifelse(s7q03 == 2, 0, 1))
```



``` r
tab_03 <- tab7_2 %>% summarise(
  N_hat = survey_total(yes_no, na.rm = TRUE, vartype = c("se", "cv")),
  P_hat  = survey_mean(yes_no, na.rm = TRUE, vartype = c("se", "cv")),
  T_hat  = survey_total(expenditure, na.rm = TRUE, vartype = c("se", "cv")),
  M_hat  = survey_mean(expenditure, na.rm = TRUE, vartype = c("se", "cv")),
  Md_hat  = survey_median(expenditure, na.rm = TRUE, vartype = c("se", "cv"))
) 

saveRDS(tab_03, "data/tablas/secc3/tab_03.rds")
```

