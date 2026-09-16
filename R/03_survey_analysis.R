
# 03_survey_analysis.R
#
# Project:
#   NHANES Health Disparities
#
# Purpose:
#   Conduct survey-weighted analyses of healthcare access
#   and cardiometabolic health among U.S. adults.
#
# Important:
#   NHANES uses a complex survey design.
#   Final estimates must account for:
#     - survey weights
#     - strata
#     - primary sampling units (PSUs)


# 1. Load packages

library(DBI)
library(RPostgres)
library(dplyr)
library(survey)
library(rstudioapi)
library(readr)


# 2. PostgreSQL connection settings

db_host <- "localhost"
db_port <- 5432
db_name <- "nhanes_health_disparities"
db_user <- "postgres"

db_password <- askForPassword(
  "Enter your PostgreSQL password"
)


# 3. Connect to PostgreSQL

con <- dbConnect(
  RPostgres::Postgres(),
  host = db_host,
  port = db_port,
  dbname = db_name,
  user = db_user,
  password = db_password
)

cat(
  "Connected to PostgreSQL database:",
  db_name,
  "\n"
)


# 4. Import analysis-ready table

analysis_df <- dbGetQuery(
  con,
  "
  SELECT *
  FROM analytics.analysis_ready
  ORDER BY cycle, participant_id;
  "
)


# 5. Disconnect from PostgreSQL

dbDisconnect(con)

cat(
  "PostgreSQL connection closed.\n"
)


# 6. Check imported dataset

cat(
  "\nTotal rows:",
  nrow(analysis_df),
  "\n"
)

cat(
  "Unique participant keys:",
  n_distinct(analysis_df$participant_key),
  "\n"
)

cat(
  "Duplicate participant keys:",
  nrow(analysis_df) -
    n_distinct(analysis_df$participant_key),
  "\n"
)


# 7. Check participants by cycle

analysis_df %>%
  count(
    cycle,
    name = "n_participants"
  ) %>%
  arrange(cycle)

# 8. Check adult sample by cycle

analysis_df %>%
  group_by(cycle) %>%
  summarise(
    total_n = n(),
    adult_n = sum(
      adult_eligible == 1,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  arrange(cycle)

# ============================================================
# STEP 8B — CREATE NHANES SURVEY DESIGNS
# ============================================================

# 9. Survey analysis settings

# This setting allows survey analyses to proceed if a
# subpopulation happens to contain only one PSU in a stratum.
options(survey.lonely.psu = "adjust")


# ----------------------------
# 10. Check survey weights
# ----------------------------

analysis_df %>%
  group_by(cycle) %>%
  summarise(
    n = n(),
    
    interview_weight_missing =
      sum(is.na(interview_weight)),
    
    interview_weight_zero =
      sum(interview_weight == 0, na.rm = TRUE),
    
    mec_weight_missing =
      sum(is.na(mec_weight)),
    
    mec_weight_zero =
      sum(mec_weight == 0, na.rm = TRUE),
    
    hba1c_weight_missing =
      sum(is.na(hba1c_weight)),
    
    hba1c_weight_zero =
      sum(hba1c_weight == 0, na.rm = TRUE),
    
    .groups = "drop"
  )

# 10A. Create HbA1c design weight

analysis_df <- analysis_df %>%
  mutate(
    hba1c_weight_design = case_when(
      
      cycle %in% c(
        "2013-2014",
        "2015-2016",
        "2017-2020"
      ) ~ mec_weight,
      
      cycle == "2021-2023" ~
        coalesce(hba1c_weight, 0),
      
      TRUE ~ NA_real_
    )
  )

# 11. Interview-weight survey designs

interview_designs <- analysis_df %>%
  split(.$cycle) %>%
  lapply(
    function(df) {
      
      svydesign(
        ids = ~psu,
        strata = ~stratum,
        weights = ~interview_weight,
        data = df,
        nest = TRUE
      )
    }
  )

# 12. MEC-weight survey designs

mec_designs <- analysis_df %>%
  split(.$cycle) %>%
  lapply(
    function(df) {
      
      svydesign(
        ids = ~psu,
        strata = ~stratum,
        weights = ~mec_weight,
        data = df,
        nest = TRUE
      )
    }
  )

# 13. HbA1c-weight survey designs

hba1c_designs <- analysis_df %>%
  split(.$cycle) %>%
  lapply(
    function(df) {
      
      svydesign(
        ids = ~psu,
        strata = ~stratum,
        weights = ~hba1c_weight_design,
        data = df,
        nest = TRUE
      )
    }
  )


# 14. Adult survey domains

adult_interview_designs <- lapply(
  interview_designs,
  function(design) {
    subset(
      design,
      adult_eligible == 1
    )
  }
)


adult_mec_designs <- lapply(
  mec_designs,
  function(design) {
    subset(
      design,
      adult_eligible == 1
    )
  }
)


adult_hba1c_designs <- lapply(
  hba1c_designs,
  function(design) {
    subset(
      design,
      adult_eligible == 1
    )
  }
)

# 15. Check adult survey-domain sizes

sapply(
  adult_interview_designs,
  function(design) {
    nrow(design$variables)
  }
)

# 16. Check positive survey weights among adults

weight_qa <- tibble(
  cycle = names(adult_interview_designs),
  
  interview_positive_weight = sapply(
    adult_interview_designs,
    function(design) {
      sum(weights(design) > 0, na.rm = TRUE)
    }
  ),
  
  mec_positive_weight = sapply(
    adult_mec_designs,
    function(design) {
      sum(weights(design) > 0, na.rm = TRUE)
    }
  ),
  
  hba1c_positive_weight = sapply(
    adult_hba1c_designs,
    function(design) {
      sum(weights(design) > 0, na.rm = TRUE)
    }
  )
)

weight_qa

# ============================================================
# STEP 8C — SURVEY-WEIGHTED ESTIMATES
# ============================================================

# 17. Function for weighted prevalence

get_weighted_prevalence <- function(
    design_list,
    variable,
    outcome_name
) {
  
  results <- lapply(
    names(design_list),
    function(cycle_name) {
      
      design <- design_list[[cycle_name]]
      
      formula <- as.formula(
        paste0("~", variable)
      )
      
      estimate <- svyciprop(
        formula,
        design = design,
        method = "logit",
        na.rm = TRUE
      )
      
      ci <- confint(estimate)
      
      tibble(
        cycle = cycle_name,
        outcome = outcome_name,
        estimate_pct =
          as.numeric(coef(estimate)) * 100,
        lower_95ci =
          as.numeric(ci[1]) * 100,
        upper_95ci =
          as.numeric(ci[2]) * 100
      )
    }
  )
  
  bind_rows(results)
}


# 18. Interview-weighted outcomes

insurance_results <- get_weighted_prevalence(
  adult_interview_designs,
  "insured",
  "Insured"
)

usual_care_results <- get_weighted_prevalence(
  adult_interview_designs,
  "has_usual_care",
  "Has usual source of care"
)

diabetes_results <- get_weighted_prevalence(
  adult_interview_designs,
  "diabetes_dx",
  "Diagnosed diabetes"
)

hypertension_results <- get_weighted_prevalence(
  adult_interview_designs,
  "hypertension_dx",
  "Diagnosed hypertension"
)

# 19. Obesity survey domains

adult_obesity_designs <- lapply(
  adult_mec_designs,
  function(design) {
    
    subset(
      design,
      obesity_eligible == 1
    )
  }
)

# 20. Survey-weighted obesity prevalence

obesity_results <- get_weighted_prevalence(
  adult_obesity_designs,
  "obesity",
  "Obesity"
)

# 21. Function for weighted mean

get_weighted_mean <- function(
    design_list,
    variable,
    outcome_name
) {
  
  results <- lapply(
    names(design_list),
    function(cycle_name) {
      
      design <- design_list[[cycle_name]]
      
      formula <- as.formula(
        paste0("~", variable)
      )
      
      estimate <- svymean(
        formula,
        design = design,
        na.rm = TRUE
      )
      
      ci <- confint(estimate)
      
      tibble(
        cycle = cycle_name,
        outcome = outcome_name,
        estimate =
          as.numeric(coef(estimate)),
        lower_95ci =
          as.numeric(ci[1]),
        upper_95ci =
          as.numeric(ci[2])
      )
    }
  )
  
  bind_rows(results)
}

# 22. Survey-weighted mean HbA1c

hba1c_results <- get_weighted_mean(
  adult_hba1c_designs,
  "hba1c",
  "Mean HbA1c"
)

# 23. View weighted results

insurance_results
usual_care_results
diabetes_results
hypertension_results
obesity_results
hba1c_results

# ============================================================
# STEP 8D — RACE/ETHNICITY DISPARITIES
# ============================================================

# 24. Race-specific weighted prevalence function

get_race_prevalence <- function(
    design_list,
    variable,
    outcome_name
) {
  
  results <- lapply(
    names(design_list),
    function(cycle_name) {
      
      design <- design_list[[cycle_name]]
      
      race_groups <- sort(
        unique(
          design$variables$race_ethnicity[
            !is.na(design$variables$race_ethnicity)
          ]
        )
      )
      
      cycle_results <- lapply(
        race_groups,
        function(race_group) {
          
          race_design <- subset(
            design,
            race_ethnicity == race_group
          )
          
          formula <- as.formula(
            paste0("~", variable)
          )
          
          estimate <- svyciprop(
            formula,
            design = race_design,
            method = "logit",
            na.rm = TRUE
          )
          
          ci <- confint(estimate)
          
          tibble(
            cycle = cycle_name,
            race_ethnicity = race_group,
            outcome = outcome_name,
            
            estimate_pct =
              as.numeric(coef(estimate)) * 100,
            
            lower_95ci =
              as.numeric(ci[1]) * 100,
            
            upper_95ci =
              as.numeric(ci[2]) * 100
          )
        }
      )
      
      bind_rows(cycle_results)
    }
  )
  
  bind_rows(results)
}

# 25. Race-specific binary outcomes

insurance_race <- get_race_prevalence(
  adult_interview_designs,
  "insured",
  "Insured"
)

usual_care_race <- get_race_prevalence(
  adult_interview_designs,
  "has_usual_care",
  "Has usual source of care"
)

diabetes_race <- get_race_prevalence(
  adult_interview_designs,
  "diabetes_dx",
  "Diagnosed diabetes"
)

hypertension_race <- get_race_prevalence(
  adult_interview_designs,
  "hypertension_dx",
  "Diagnosed hypertension"
)

obesity_race <- get_race_prevalence(
  adult_obesity_designs,
  "obesity",
  "Obesity"
)

# 26. Race-specific weighted mean function

get_race_mean <- function(
    design_list,
    variable,
    outcome_name
) {
  
  results <- lapply(
    names(design_list),
    function(cycle_name) {
      
      design <- design_list[[cycle_name]]
      
      race_groups <- sort(
        unique(
          design$variables$race_ethnicity[
            !is.na(design$variables$race_ethnicity)
          ]
        )
      )
      
      cycle_results <- lapply(
        race_groups,
        function(race_group) {
          
          race_design <- subset(
            design,
            race_ethnicity == race_group
          )
          
          formula <- as.formula(
            paste0("~", variable)
          )
          
          estimate <- svymean(
            formula,
            design = race_design,
            na.rm = TRUE
          )
          
          ci <- confint(estimate)
          
          tibble(
            cycle = cycle_name,
            race_ethnicity = race_group,
            outcome = outcome_name,
            
            estimate =
              as.numeric(coef(estimate)),
            
            lower_95ci =
              as.numeric(ci[1]),
            
            upper_95ci =
              as.numeric(ci[2])
          )
        }
      )
      
      bind_rows(cycle_results)
    }
  )
  
  bind_rows(results)
}


hba1c_race <- get_race_mean(
  adult_hba1c_designs,
  "hba1c",
  "Mean HbA1c"
)

# 27. View race-specific estimates

insurance_race
usual_care_race
diabetes_race
hypertension_race
obesity_race
hba1c_race


# ============================================================
# 28. SUBGROUP RELIABILITY / SAMPLE-SIZE CHECK
# ============================================================

# 28A. Function for binary outcomes

get_binary_reliability <- function(
    design_list,
    variable,
    outcome_name
) {
  
  results <- lapply(
    names(design_list),
    function(cycle_name) {
      
      design <- design_list[[cycle_name]]
      
      race_groups <- sort(
        unique(
          design$variables$race_ethnicity[
            !is.na(design$variables$race_ethnicity)
          ]
        )
      )
      
      cycle_results <- lapply(
        race_groups,
        function(race_group) {
          
          race_design <- subset(
            design,
            race_ethnicity == race_group
          )
          
          # Which observations actually contribute?
          observed <-
            !is.na(race_design$variables[[variable]]) &
            weights(race_design) > 0
          
          n_unweighted <- sum(observed)
          
          formula <- as.formula(
            paste0("~", variable)
          )
          
          estimate <- svyciprop(
            formula,
            design = race_design,
            method = "logit",
            na.rm = TRUE
          )
          
          ci <- confint(estimate)
          
          estimate_value <-
            as.numeric(coef(estimate))
          
          se_value <-
            as.numeric(SE(estimate))
          
          rse_pct <- ifelse(
            estimate_value > 0,
            100 * se_value / estimate_value,
            NA_real_
          )
          
          tibble(
            cycle = cycle_name,
            race_ethnicity = race_group,
            outcome = outcome_name,
            
            n_unweighted = n_unweighted,
            
            estimate_pct =
              estimate_value * 100,
            
            se_pct =
              se_value * 100,
            
            rse_pct = rse_pct,
            
            lower_95ci =
              as.numeric(ci[1]) * 100,
            
            upper_95ci =
              as.numeric(ci[2]) * 100,
            
            ci_width_pp =
              (as.numeric(ci[2]) -
                 as.numeric(ci[1])) * 100,
            
            reliability_flag = case_when(
              n_unweighted < 30 ~
                "Review: n < 30",
              
              rse_pct > 30 ~
                "Review: RSE > 30%",
              
              TRUE ~
                "OK"
            )
          )
        }
      )
      
      bind_rows(cycle_results)
    }
  )
  
  bind_rows(results)
}

# 28B. Run binary reliability checks

insurance_reliability <- get_binary_reliability(
  adult_interview_designs,
  "insured",
  "Insured"
)

usual_care_reliability <- get_binary_reliability(
  adult_interview_designs,
  "has_usual_care",
  "Has usual source of care"
)

diabetes_reliability <- get_binary_reliability(
  adult_interview_designs,
  "diabetes_dx",
  "Diagnosed diabetes"
)

hypertension_reliability <- get_binary_reliability(
  adult_interview_designs,
  "hypertension_dx",
  "Diagnosed hypertension"
)

obesity_reliability <- get_binary_reliability(
  adult_obesity_designs,
  "obesity",
  "Obesity"
)

binary_reliability <- bind_rows(
  insurance_reliability,
  usual_care_reliability,
  diabetes_reliability,
  hypertension_reliability,
  obesity_reliability
)

binary_reliability %>%
  filter(reliability_flag != "OK") %>%
  print(n = Inf)

get_hba1c_reliability <- function(
    design_list
) {
  
  results <- lapply(
    names(design_list),
    function(cycle_name) {
      
      design <- design_list[[cycle_name]]
      
      race_groups <- sort(
        unique(
          design$variables$race_ethnicity[
            !is.na(design$variables$race_ethnicity)
          ]
        )
      )
      
      cycle_results <- lapply(
        race_groups,
        function(race_group) {
          
          race_design <- subset(
            design,
            race_ethnicity == race_group
          )
          
          observed <-
            !is.na(race_design$variables$hba1c) &
            weights(race_design) > 0
          
          n_unweighted <- sum(observed)
          
          estimate <- svymean(
            ~hba1c,
            design = race_design,
            na.rm = TRUE
          )
          
          ci <- confint(estimate)
          
          tibble(
            cycle = cycle_name,
            race_ethnicity = race_group,
            
            n_unweighted = n_unweighted,
            
            mean_hba1c =
              as.numeric(coef(estimate)),
            
            standard_error =
              as.numeric(SE(estimate)),
            
            lower_95ci =
              as.numeric(ci[1]),
            
            upper_95ci =
              as.numeric(ci[2]),
            
            ci_width =
              as.numeric(ci[2]) -
              as.numeric(ci[1]),
            
            reliability_flag = case_when(
              n_unweighted < 30 ~
                "Review: n < 30",
              
              TRUE ~
                "OK"
            )
          )
        }
      )
      
      bind_rows(cycle_results)
    }
  )
  
  bind_rows(results)
}


hba1c_reliability <- get_hba1c_reliability(
  adult_hba1c_designs
)

hba1c_reliability %>%
  filter(reliability_flag != "OK") %>%
  print(n = Inf)

# ============================================================
# STEP 8E — SURVEY-WEIGHTED REGRESSION / FORMAL TESTING
# ============================================================

# 29. Prepare variables for trend modeling

model_df <- analysis_df %>%
  mutate(
    
    # Time measured in 2-year units from 2013-2014
    #
    # 2013-2014       = 0
    # 2015-2016       = 1
    # 2017-Mar 2020   = 2.3
    # 2021-2023       = 4.35
    #
    # This preserves the unequal spacing between periods.
    time_2yr = (time_midpoint - 2014) / 2,
    
    # Make race/ethnicity a factor and use
    # Non-Hispanic White as the reference group.
    race_ethnicity = relevel(
      factor(race_ethnicity),
      ref = "Non-Hispanic White"
    ),
    
    # Create cycle-specific survey design identifiers.
    # Stratum/PSU numbers can repeat across NHANES cycles,
    # so they must be made unique before stacking cycles.
    stratum_cycle = interaction(
      cycle,
      stratum,
      drop = TRUE
    ),
    
    psu_cycle = interaction(
      cycle,
      stratum,
      psu,
      drop = TRUE
    )
  )

# 30. Create stacked survey designs for trend models

# Interview-weight design
trend_interview_design <- svydesign(
  ids = ~psu_cycle,
  strata = ~stratum_cycle,
  weights = ~interview_weight,
  data = model_df,
  nest = TRUE
)


# MEC-weight design
trend_mec_design <- svydesign(
  ids = ~psu_cycle,
  strata = ~stratum_cycle,
  weights = ~mec_weight,
  data = model_df,
  nest = TRUE
)


# HbA1c-weight design
trend_hba1c_design <- svydesign(
  ids = ~psu_cycle,
  strata = ~stratum_cycle,
  weights = ~hba1c_weight_design,
  data = model_df,
  nest = TRUE
)

# 31. Define adult modeling domains

adult_trend_interview <- subset(
  trend_interview_design,
  adult_eligible == 1
)


adult_trend_mec <- subset(
  trend_mec_design,
  adult_eligible == 1
)


adult_trend_hba1c <- subset(
  trend_hba1c_design,
  adult_eligible == 1
)


adult_trend_obesity <- subset(
  trend_mec_design,
  adult_eligible == 1 &
    obesity_eligible == 1
)

# 32. Check modeling domains

tibble(
  design = c(
    "Interview",
    "MEC",
    "HbA1c",
    "Obesity"
  ),
  
  n_records = c(
    nrow(adult_trend_interview$variables),
    nrow(adult_trend_mec$variables),
    nrow(adult_trend_hba1c$variables),
    nrow(adult_trend_obesity$variables)
  ),
  
  n_positive_weight = c(
    sum(weights(adult_trend_interview) > 0),
    sum(weights(adult_trend_mec) > 0),
    sum(weights(adult_trend_hba1c) > 0),
    sum(weights(adult_trend_obesity) > 0)
  )
)

# 33. Overall binary-outcome trend models

insurance_trend_model <- svyglm(
  insured ~ time_2yr,
  design = adult_trend_interview,
  family = quasibinomial()
)


usual_care_trend_model <- svyglm(
  has_usual_care ~ time_2yr,
  design = adult_trend_interview,
  family = quasibinomial()
)


diabetes_trend_model <- svyglm(
  diabetes_dx ~ time_2yr,
  design = adult_trend_interview,
  family = quasibinomial()
)


hypertension_trend_model <- svyglm(
  hypertension_dx ~ time_2yr,
  design = adult_trend_interview,
  family = quasibinomial()
)


obesity_trend_model <- svyglm(
  obesity ~ time_2yr,
  design = adult_trend_obesity,
  family = quasibinomial()
)

# 34. Overall HbA1c trend model

hba1c_trend_model <- svyglm(
  hba1c ~ time_2yr,
  design = adult_trend_hba1c
)

# 35. Extract binary trend results

extract_binary_trend <- function(
    model,
    outcome_name
) {
  
  coef_table <- summary(model)$coefficients
  
  ci <- confint(model)
  
  beta <-
    as.numeric(
      coef(model)["time_2yr"]
    )
  
  tibble(
    outcome = outcome_name,
    
    beta =
      beta,
    
    odds_ratio_per_2yr =
      exp(beta),
    
    lower_95ci =
      exp(ci["time_2yr", 1]),
    
    upper_95ci =
      exp(ci["time_2yr", 2]),
    
    p_value =
      coef_table[
        "time_2yr",
        "Pr(>|t|)"
      ]
  )
}

binary_trend_results <- bind_rows(
  
  extract_binary_trend(
    insurance_trend_model,
    "Insured"
  ),
  
  extract_binary_trend(
    usual_care_trend_model,
    "Has usual source of care"
  ),
  
  extract_binary_trend(
    diabetes_trend_model,
    "Diagnosed diabetes"
  ),
  
  extract_binary_trend(
    hypertension_trend_model,
    "Diagnosed hypertension"
  ),
  
  extract_binary_trend(
    obesity_trend_model,
    "Obesity"
  )
)


binary_trend_results

# 36. Extract HbA1c trend result

hba1c_coef <-
  summary(hba1c_trend_model)$coefficients

hba1c_ci <-
  confint(hba1c_trend_model)


hba1c_trend_result <- tibble(
  
  outcome = "Mean HbA1c",
  
  change_per_2yr =
    as.numeric(
      coef(hba1c_trend_model)["time_2yr"]
    ),
  
  lower_95ci =
    hba1c_ci["time_2yr", 1],
  
  upper_95ci =
    hba1c_ci["time_2yr", 2],
  
  p_value =
    hba1c_coef[
      "time_2yr",
      "Pr(>|t|)"
    ]
)


hba1c_trend_result

# ============================================================
# STEP 8E-2 — RACE/ETHNICITY + RACE × TIME
# ============================================================

# 37. Binary models: race + time

insurance_race_model <- svyglm(
  insured ~ time_2yr + race_ethnicity,
  design = adult_trend_interview,
  family = quasibinomial()
)

usual_care_race_model <- svyglm(
  has_usual_care ~ time_2yr + race_ethnicity,
  design = adult_trend_interview,
  family = quasibinomial()
)

diabetes_race_model <- svyglm(
  diabetes_dx ~ time_2yr + race_ethnicity,
  design = adult_trend_interview,
  family = quasibinomial()
)

hypertension_race_model <- svyglm(
  hypertension_dx ~ time_2yr + race_ethnicity,
  design = adult_trend_interview,
  family = quasibinomial()
)

obesity_race_model <- svyglm(
  obesity ~ time_2yr + race_ethnicity,
  design = adult_trend_obesity,
  family = quasibinomial()
)

# 38. HbA1c model: race + time

hba1c_race_model <- svyglm(
  hba1c ~ time_2yr + race_ethnicity,
  design = adult_trend_hba1c
)

# 39. Global tests for race/ethnicity

regTermTest(
  insurance_race_model,
  ~race_ethnicity
)

regTermTest(
  usual_care_race_model,
  ~race_ethnicity
)

regTermTest(
  diabetes_race_model,
  ~race_ethnicity
)

regTermTest(
  hypertension_race_model,
  ~race_ethnicity
)

regTermTest(
  obesity_race_model,
  ~race_ethnicity
)

regTermTest(
  hba1c_race_model,
  ~race_ethnicity
)

# 40. Binary race × time interaction models

insurance_interaction_model <- svyglm(
  insured ~ time_2yr * race_ethnicity,
  design = adult_trend_interview,
  family = quasibinomial()
)

usual_care_interaction_model <- svyglm(
  has_usual_care ~ time_2yr * race_ethnicity,
  design = adult_trend_interview,
  family = quasibinomial()
)

diabetes_interaction_model <- svyglm(
  diabetes_dx ~ time_2yr * race_ethnicity,
  design = adult_trend_interview,
  family = quasibinomial()
)

hypertension_interaction_model <- svyglm(
  hypertension_dx ~ time_2yr * race_ethnicity,
  design = adult_trend_interview,
  family = quasibinomial()
)

obesity_interaction_model <- svyglm(
  obesity ~ time_2yr * race_ethnicity,
  design = adult_trend_obesity,
  family = quasibinomial()
)

# 41. HbA1c race × time interaction model

hba1c_interaction_model <- svyglm(
  hba1c ~ time_2yr * race_ethnicity,
  design = adult_trend_hba1c
)

# 42. Global race × time tests

regTermTest(
  insurance_interaction_model,
  ~time_2yr:race_ethnicity
)

regTermTest(
  usual_care_interaction_model,
  ~time_2yr:race_ethnicity
)

regTermTest(
  diabetes_interaction_model,
  ~time_2yr:race_ethnicity
)

regTermTest(
  hypertension_interaction_model,
  ~time_2yr:race_ethnicity
)

regTermTest(
  obesity_interaction_model,
  ~time_2yr:race_ethnicity
)

regTermTest(
  hba1c_interaction_model,
  ~time_2yr:race_ethnicity
)

# ============================================================
# STEP 8E-3 — ADJUSTED MODELS
# ============================================================

# 43. QA adjustment variables

model_df %>%
  filter(adult_eligible == 1) %>%
  summarise(
    n_adults = n(),
    
    age_group_missing =
      sum(is.na(age_group)),
    
    sex_missing =
      sum(is.na(sex)),
    
    education_missing =
      sum(is.na(education_group)),
    
    income_missing =
      sum(is.na(income_group))
  )

# 44. Model 1: age + sex adjusted

insurance_adj1 <- svyglm(
  insured ~
    time_2yr +
    race_ethnicity +
    factor(age_group) +
    factor(sex),
  design = adult_trend_interview,
  family = quasibinomial()
)


usual_care_adj1 <- svyglm(
  has_usual_care ~
    time_2yr +
    race_ethnicity +
    factor(age_group) +
    factor(sex),
  design = adult_trend_interview,
  family = quasibinomial()
)


diabetes_adj1 <- svyglm(
  diabetes_dx ~
    time_2yr +
    race_ethnicity +
    factor(age_group) +
    factor(sex),
  design = adult_trend_interview,
  family = quasibinomial()
)


hypertension_adj1 <- svyglm(
  hypertension_dx ~
    time_2yr +
    race_ethnicity +
    factor(age_group) +
    factor(sex),
  design = adult_trend_interview,
  family = quasibinomial()
)


obesity_adj1 <- svyglm(
  obesity ~
    time_2yr +
    race_ethnicity +
    factor(age_group) +
    factor(sex),
  design = adult_trend_obesity,
  family = quasibinomial()
)

hba1c_adj1 <- svyglm(
  hba1c ~
    time_2yr +
    race_ethnicity +
    factor(age_group) +
    factor(sex),
  design = adult_trend_hba1c
)

# 45. Model 2: demographic + socioeconomic adjustment

insurance_adj2 <- svyglm(
  insured ~
    time_2yr +
    race_ethnicity +
    factor(age_group) +
    factor(sex) +
    factor(education_group) +
    factor(income_group),
  design = adult_trend_interview,
  family = quasibinomial()
)


usual_care_adj2 <- svyglm(
  has_usual_care ~
    time_2yr +
    race_ethnicity +
    factor(age_group) +
    factor(sex) +
    factor(education_group) +
    factor(income_group),
  design = adult_trend_interview,
  family = quasibinomial()
)


diabetes_adj2 <- svyglm(
  diabetes_dx ~
    time_2yr +
    race_ethnicity +
    factor(age_group) +
    factor(sex) +
    factor(education_group) +
    factor(income_group),
  design = adult_trend_interview,
  family = quasibinomial()
)


hypertension_adj2 <- svyglm(
  hypertension_dx ~
    time_2yr +
    race_ethnicity +
    factor(age_group) +
    factor(sex) +
    factor(education_group) +
    factor(income_group),
  design = adult_trend_interview,
  family = quasibinomial()
)


obesity_adj2 <- svyglm(
  obesity ~
    time_2yr +
    race_ethnicity +
    factor(age_group) +
    factor(sex) +
    factor(education_group) +
    factor(income_group),
  design = adult_trend_obesity,
  family = quasibinomial()
)


hba1c_adj2 <- svyglm(
  hba1c ~
    time_2yr +
    race_ethnicity +
    factor(age_group) +
    factor(sex) +
    factor(education_group) +
    factor(income_group),
  design = adult_trend_hba1c
)

# 46. Global adjusted race/ethnicity tests

# Model 1
regTermTest(insurance_adj1, ~race_ethnicity)
regTermTest(usual_care_adj1, ~race_ethnicity)
regTermTest(diabetes_adj1, ~race_ethnicity)
regTermTest(hypertension_adj1, ~race_ethnicity)
regTermTest(obesity_adj1, ~race_ethnicity)
regTermTest(hba1c_adj1, ~race_ethnicity)


# Model 2
regTermTest(insurance_adj2, ~race_ethnicity)
regTermTest(usual_care_adj2, ~race_ethnicity)
regTermTest(diabetes_adj2, ~race_ethnicity)
regTermTest(hypertension_adj2, ~race_ethnicity)
regTermTest(obesity_adj2, ~race_ethnicity)
regTermTest(hba1c_adj2, ~race_ethnicity)


# 47. Adjusted time trends

adjusted_trend_results <- bind_rows(
  
  extract_binary_trend(
    insurance_adj2,
    "Insured"
  ),
  
  extract_binary_trend(
    usual_care_adj2,
    "Has usual source of care"
  ),
  
  extract_binary_trend(
    diabetes_adj2,
    "Diagnosed diabetes"
  ),
  
  extract_binary_trend(
    hypertension_adj2,
    "Diagnosed hypertension"
  ),
  
  extract_binary_trend(
    obesity_adj2,
    "Obesity"
  )
)

adjusted_trend_results

hba1c_adj2_coef <-
  summary(hba1c_adj2)$coefficients

hba1c_adj2_ci <-
  confint(hba1c_adj2)


hba1c_adjusted_trend <- tibble(
  
  outcome = "Mean HbA1c",
  
  change_per_2yr =
    as.numeric(
      coef(hba1c_adj2)["time_2yr"]
    ),
  
  lower_95ci =
    hba1c_adj2_ci["time_2yr", 1],
  
  upper_95ci =
    hba1c_adj2_ci["time_2yr", 2],
  
  p_value =
    hba1c_adj2_coef[
      "time_2yr",
      "Pr(>|t|)"
    ]
)

hba1c_adjusted_trend

# 48. Check analytic sample sizes for adjusted models

adjusted_model_sample_sizes <- tibble(
  outcome = c(
    "Insured",
    "Has usual source of care",
    "Diagnosed diabetes",
    "Diagnosed hypertension",
    "Obesity",
    "Mean HbA1c"
  ),
  
  model1_n = c(
    nobs(insurance_adj1),
    nobs(usual_care_adj1),
    nobs(diabetes_adj1),
    nobs(hypertension_adj1),
    nobs(obesity_adj1),
    nobs(hba1c_adj1)
  ),
  
  model2_n = c(
    nobs(insurance_adj2),
    nobs(usual_care_adj2),
    nobs(diabetes_adj2),
    nobs(hypertension_adj2),
    nobs(obesity_adj2),
    nobs(hba1c_adj2)
  )
)

adjusted_model_sample_sizes

# ============================================================
# STEP 8E-4 — ADJUSTED RACE × TIME INTERACTIONS
# ============================================================

# 49. Fully adjusted interaction models

insurance_adj_interaction <- svyglm(
  insured ~
    time_2yr * race_ethnicity +
    factor(age_group) +
    factor(sex) +
    factor(education_group) +
    factor(income_group),
  design = adult_trend_interview,
  family = quasibinomial()
)


usual_care_adj_interaction <- svyglm(
  has_usual_care ~
    time_2yr * race_ethnicity +
    factor(age_group) +
    factor(sex) +
    factor(education_group) +
    factor(income_group),
  design = adult_trend_interview,
  family = quasibinomial()
)


diabetes_adj_interaction <- svyglm(
  diabetes_dx ~
    time_2yr * race_ethnicity +
    factor(age_group) +
    factor(sex) +
    factor(education_group) +
    factor(income_group),
  design = adult_trend_interview,
  family = quasibinomial()
)


hypertension_adj_interaction <- svyglm(
  hypertension_dx ~
    time_2yr * race_ethnicity +
    factor(age_group) +
    factor(sex) +
    factor(education_group) +
    factor(income_group),
  design = adult_trend_interview,
  family = quasibinomial()
)


obesity_adj_interaction <- svyglm(
  obesity ~
    time_2yr * race_ethnicity +
    factor(age_group) +
    factor(sex) +
    factor(education_group) +
    factor(income_group),
  design = adult_trend_obesity,
  family = quasibinomial()
)


hba1c_adj_interaction <- svyglm(
  hba1c ~
    time_2yr * race_ethnicity +
    factor(age_group) +
    factor(sex) +
    factor(education_group) +
    factor(income_group),
  design = adult_trend_hba1c
)

# 50. Global adjusted race × time tests

regTermTest(
  insurance_adj_interaction,
  ~time_2yr:race_ethnicity
)

regTermTest(
  usual_care_adj_interaction,
  ~time_2yr:race_ethnicity
)

regTermTest(
  diabetes_adj_interaction,
  ~time_2yr:race_ethnicity
)

regTermTest(
  hypertension_adj_interaction,
  ~time_2yr:race_ethnicity
)

regTermTest(
  obesity_adj_interaction,
  ~time_2yr:race_ethnicity
)

regTermTest(
  hba1c_adj_interaction,
  ~time_2yr:race_ethnicity
)

# ============================================================
# STEP 8E-5 — HYPERTENSION INTERACTION SENSITIVITY CHECK
# ============================================================

# 51. Create common complete-case hypertension domain

hypertension_complete_case <- subset(
  adult_trend_interview,
  
  !is.na(hypertension_dx) &
    !is.na(education_group) &
    !is.na(income_group)
)


nrow(hypertension_complete_case$variables)

# 52. Compare hypertension interaction models
#      using identical participants

# Model A:
# race × time only
hypertension_cc_unadjusted <- svyglm(
  hypertension_dx ~
    time_2yr * race_ethnicity,
  
  design = hypertension_complete_case,
  
  family = quasibinomial()
)


# Model B:
# + age and sex
hypertension_cc_adj1 <- svyglm(
  hypertension_dx ~
    time_2yr * race_ethnicity +
    factor(age_group) +
    factor(sex),
  
  design = hypertension_complete_case,
  
  family = quasibinomial()
)


# Model C:
# + education and income
hypertension_cc_adj2 <- svyglm(
  hypertension_dx ~
    time_2yr * race_ethnicity +
    factor(age_group) +
    factor(sex) +
    factor(education_group) +
    factor(income_group),
  
  design = hypertension_complete_case,
  
  family = quasibinomial()
)

# 53. Global race × time tests
#     on the same analytic sample

regTermTest(
  hypertension_cc_unadjusted,
  ~time_2yr:race_ethnicity
)

regTermTest(
  hypertension_cc_adj1,
  ~time_2yr:race_ethnicity
)

regTermTest(
  hypertension_cc_adj2,
  ~time_2yr:race_ethnicity
)

# ============================================================
# STEP 8F — FINAL RESULT TABLES / EXPORTS
# ============================================================

# 54. Survey-cycle metadata

cycle_lookup <- tibble(
  cycle = c(
    "2013-2014",
    "2015-2016",
    "2017-2020",
    "2021-2023"
  ),
  
  cycle_label = c(
    "2013–2014",
    "2015–2016",
    "2017–March 2020",
    "2021–2023"
  ),
  
  time_midpoint = c(
    2014.0,
    2016.0,
    2018.6,
    2022.7
  ),
  
  cycle_order = 1:4
)

# 55. Overall survey-weighted estimates

overall_estimates <- bind_rows(
  
  insurance_results %>%
    transmute(
      cycle,
      outcome,
      estimate = estimate_pct,
      lower_95ci,
      upper_95ci,
      measure = "Prevalence",
      unit = "Percent"
    ),
  
  usual_care_results %>%
    transmute(
      cycle,
      outcome,
      estimate = estimate_pct,
      lower_95ci,
      upper_95ci,
      measure = "Prevalence",
      unit = "Percent"
    ),
  
  diabetes_results %>%
    transmute(
      cycle,
      outcome,
      estimate = estimate_pct,
      lower_95ci,
      upper_95ci,
      measure = "Prevalence",
      unit = "Percent"
    ),
  
  hypertension_results %>%
    transmute(
      cycle,
      outcome,
      estimate = estimate_pct,
      lower_95ci,
      upper_95ci,
      measure = "Prevalence",
      unit = "Percent"
    ),
  
  obesity_results %>%
    transmute(
      cycle,
      outcome,
      estimate = estimate_pct,
      lower_95ci,
      upper_95ci,
      measure = "Prevalence",
      unit = "Percent"
    ),
  
  hba1c_results %>%
    transmute(
      cycle,
      outcome,
      estimate,
      lower_95ci,
      upper_95ci,
      measure = "Mean",
      unit = "HbA1c percent"
    )
) %>%
  
  left_join(
    cycle_lookup,
    by = "cycle"
  ) %>%
  
  arrange(
    outcome,
    cycle_order
  )

overall_estimates

# 56. Race/ethnicity-specific estimates

race_estimates <- bind_rows(
  
  insurance_race %>%
    transmute(
      cycle,
      race_ethnicity,
      outcome,
      estimate = estimate_pct,
      lower_95ci,
      upper_95ci,
      measure = "Prevalence",
      unit = "Percent"
    ),
  
  usual_care_race %>%
    transmute(
      cycle,
      race_ethnicity,
      outcome,
      estimate = estimate_pct,
      lower_95ci,
      upper_95ci,
      measure = "Prevalence",
      unit = "Percent"
    ),
  
  diabetes_race %>%
    transmute(
      cycle,
      race_ethnicity,
      outcome,
      estimate = estimate_pct,
      lower_95ci,
      upper_95ci,
      measure = "Prevalence",
      unit = "Percent"
    ),
  
  hypertension_race %>%
    transmute(
      cycle,
      race_ethnicity,
      outcome,
      estimate = estimate_pct,
      lower_95ci,
      upper_95ci,
      measure = "Prevalence",
      unit = "Percent"
    ),
  
  obesity_race %>%
    transmute(
      cycle,
      race_ethnicity,
      outcome,
      estimate = estimate_pct,
      lower_95ci,
      upper_95ci,
      measure = "Prevalence",
      unit = "Percent"
    ),
  
  hba1c_race %>%
    transmute(
      cycle,
      race_ethnicity,
      outcome,
      estimate,
      lower_95ci,
      upper_95ci,
      measure = "Mean",
      unit = "HbA1c percent"
    )
) %>%
  
  left_join(
    cycle_lookup,
    by = "cycle"
  ) %>%
  
  arrange(
    outcome,
    race_ethnicity,
    cycle_order
  )

nrow(race_estimates)


# 57A. HbA1c trend extraction helper

extract_hba1c_trend <- function(
    model,
    model_name
) {
  
  coef_table <- summary(model)$coefficients
  ci <- confint(model)
  
  beta <-
    as.numeric(
      coef(model)["time_2yr"]
    )
  
  p_value <-
    coef_table[
      "time_2yr",
      "Pr(>|t|)"
    ]
  
  tibble(
    outcome = "Mean HbA1c",
    
    model = model_name,
    
    effect_type = "Mean change per 2 years",
    
    estimate = beta,
    
    lower_95ci =
      ci["time_2yr", 1],
    
    upper_95ci =
      ci["time_2yr", 2],
    
    p_value =
      as.numeric(p_value)
  )
}

# 57B. Binary trend extraction helper

extract_binary_trend_tidy <- function(
    model,
    outcome_name,
    model_name
) {
  
  coef_table <- summary(model)$coefficients
  ci <- confint(model)
  
  beta <-
    as.numeric(
      coef(model)["time_2yr"]
    )
  
  tibble(
    outcome = outcome_name,
    model = model_name,
    effect_type = "Odds ratio per 2 years",
    
    estimate =
      exp(beta),
    
    lower_95ci =
      exp(
        ci["time_2yr", 1]
      ),
    
    upper_95ci =
      exp(
        ci["time_2yr", 2]
      ),
    
    p_value =
      coef_table[
        "time_2yr",
        "Pr(>|t|)"
      ]
  )
}

# 57C. Final trend-model summary

trend_model_summary <- bind_rows(
  
  # --------------------------------
  # Unadjusted
  # --------------------------------
  
  extract_binary_trend_tidy(
    insurance_trend_model,
    "Insured",
    "Unadjusted"
  ),
  
  extract_binary_trend_tidy(
    usual_care_trend_model,
    "Has usual source of care",
    "Unadjusted"
  ),
  
  extract_binary_trend_tidy(
    diabetes_trend_model,
    "Diagnosed diabetes",
    "Unadjusted"
  ),
  
  extract_binary_trend_tidy(
    hypertension_trend_model,
    "Diagnosed hypertension",
    "Unadjusted"
  ),
  
  extract_binary_trend_tidy(
    obesity_trend_model,
    "Obesity",
    "Unadjusted"
  ),
  
  extract_hba1c_trend(
    hba1c_trend_model,
    "Unadjusted"
  ),
  
  
  # --------------------------------
  # Age + sex
  # --------------------------------
  
  extract_binary_trend_tidy(
    insurance_adj1,
    "Insured",
    "Age + sex adjusted"
  ),
  
  extract_binary_trend_tidy(
    usual_care_adj1,
    "Has usual source of care",
    "Age + sex adjusted"
  ),
  
  extract_binary_trend_tidy(
    diabetes_adj1,
    "Diagnosed diabetes",
    "Age + sex adjusted"
  ),
  
  extract_binary_trend_tidy(
    hypertension_adj1,
    "Diagnosed hypertension",
    "Age + sex adjusted"
  ),
  
  extract_binary_trend_tidy(
    obesity_adj1,
    "Obesity",
    "Age + sex adjusted"
  ),
  
  extract_hba1c_trend(
    hba1c_adj1,
    "Age + sex adjusted"
  ),
  
  
  # --------------------------------
  # Fully adjusted
  # --------------------------------
  
  extract_binary_trend_tidy(
    insurance_adj2,
    "Insured",
    "Age + sex + education + income adjusted"
  ),
  
  extract_binary_trend_tidy(
    usual_care_adj2,
    "Has usual source of care",
    "Age + sex + education + income adjusted"
  ),
  
  extract_binary_trend_tidy(
    diabetes_adj2,
    "Diagnosed diabetes",
    "Age + sex + education + income adjusted"
  ),
  
  extract_binary_trend_tidy(
    hypertension_adj2,
    "Diagnosed hypertension",
    "Age + sex + education + income adjusted"
  ),
  
  extract_binary_trend_tidy(
    obesity_adj2,
    "Obesity",
    "Age + sex + education + income adjusted"
  ),
  
  extract_hba1c_trend(
    hba1c_adj2,
    "Age + sex + education + income adjusted"
  )
)

trend_model_summary

# 58A. Helper for global Wald tests

extract_global_test <- function(
    model,
    term,
    outcome_name,
    model_name,
    test_name
) {
  
  test_result <- regTermTest(
    model,
    term
  )
  
  p_value <-
    as.numeric(
      test_result$p
    )
  
  tibble(
    outcome = outcome_name,
    model = model_name,
    test = test_name,
    p_value = p_value
  )
}

# 58B. Global race/ethnicity tests

global_race_tests <- bind_rows(
  
  # Age + sex
  extract_global_test(
    insurance_adj1,
    ~race_ethnicity,
    "Insured",
    "Age + sex adjusted",
    "Race/ethnicity"
  ),
  
  extract_global_test(
    usual_care_adj1,
    ~race_ethnicity,
    "Has usual source of care",
    "Age + sex adjusted",
    "Race/ethnicity"
  ),
  
  extract_global_test(
    diabetes_adj1,
    ~race_ethnicity,
    "Diagnosed diabetes",
    "Age + sex adjusted",
    "Race/ethnicity"
  ),
  
  extract_global_test(
    hypertension_adj1,
    ~race_ethnicity,
    "Diagnosed hypertension",
    "Age + sex adjusted",
    "Race/ethnicity"
  ),
  
  extract_global_test(
    obesity_adj1,
    ~race_ethnicity,
    "Obesity",
    "Age + sex adjusted",
    "Race/ethnicity"
  ),
  
  extract_global_test(
    hba1c_adj1,
    ~race_ethnicity,
    "Mean HbA1c",
    "Age + sex adjusted",
    "Race/ethnicity"
  ),
  
  # Fully adjusted
  extract_global_test(
    insurance_adj2,
    ~race_ethnicity,
    "Insured",
    "Fully adjusted",
    "Race/ethnicity"
  ),
  
  extract_global_test(
    usual_care_adj2,
    ~race_ethnicity,
    "Has usual source of care",
    "Fully adjusted",
    "Race/ethnicity"
  ),
  
  extract_global_test(
    diabetes_adj2,
    ~race_ethnicity,
    "Diagnosed diabetes",
    "Fully adjusted",
    "Race/ethnicity"
  ),
  
  extract_global_test(
    hypertension_adj2,
    ~race_ethnicity,
    "Diagnosed hypertension",
    "Fully adjusted",
    "Race/ethnicity"
  ),
  
  extract_global_test(
    obesity_adj2,
    ~race_ethnicity,
    "Obesity",
    "Fully adjusted",
    "Race/ethnicity"
  ),
  
  extract_global_test(
    hba1c_adj2,
    ~race_ethnicity,
    "Mean HbA1c",
    "Fully adjusted",
    "Race/ethnicity"
  )
)


# 59. Global race × time interaction tests

interaction_test_summary <- bind_rows(
  
  # Unadjusted interaction models
  extract_global_test(
    insurance_interaction_model,
    ~time_2yr:race_ethnicity,
    "Insured",
    "Unadjusted interaction",
    "Race × time"
  ),
  
  extract_global_test(
    usual_care_interaction_model,
    ~time_2yr:race_ethnicity,
    "Has usual source of care",
    "Unadjusted interaction",
    "Race × time"
  ),
  
  extract_global_test(
    diabetes_interaction_model,
    ~time_2yr:race_ethnicity,
    "Diagnosed diabetes",
    "Unadjusted interaction",
    "Race × time"
  ),
  
  extract_global_test(
    hypertension_interaction_model,
    ~time_2yr:race_ethnicity,
    "Diagnosed hypertension",
    "Unadjusted interaction",
    "Race × time"
  ),
  
  extract_global_test(
    obesity_interaction_model,
    ~time_2yr:race_ethnicity,
    "Obesity",
    "Unadjusted interaction",
    "Race × time"
  ),
  
  extract_global_test(
    hba1c_interaction_model,
    ~time_2yr:race_ethnicity,
    "Mean HbA1c",
    "Unadjusted interaction",
    "Race × time"
  ),
  
  
  # Fully adjusted
  extract_global_test(
    insurance_adj_interaction,
    ~time_2yr:race_ethnicity,
    "Insured",
    "Fully adjusted interaction",
    "Race × time"
  ),
  
  extract_global_test(
    usual_care_adj_interaction,
    ~time_2yr:race_ethnicity,
    "Has usual source of care",
    "Fully adjusted interaction",
    "Race × time"
  ),
  
  extract_global_test(
    diabetes_adj_interaction,
    ~time_2yr:race_ethnicity,
    "Diagnosed diabetes",
    "Fully adjusted interaction",
    "Race × time"
  ),
  
  extract_global_test(
    hypertension_adj_interaction,
    ~time_2yr:race_ethnicity,
    "Diagnosed hypertension",
    "Fully adjusted interaction",
    "Race × time"
  ),
  
  extract_global_test(
    obesity_adj_interaction,
    ~time_2yr:race_ethnicity,
    "Obesity",
    "Fully adjusted interaction",
    "Race × time"
  ),
  
  extract_global_test(
    hba1c_adj_interaction,
    ~time_2yr:race_ethnicity,
    "Mean HbA1c",
    "Fully adjusted interaction",
    "Race × time"
  )
)

# 60. Hypertension sensitivity results

hypertension_sensitivity <- bind_rows(
  
  extract_global_test(
    hypertension_cc_unadjusted,
    ~time_2yr:race_ethnicity,
    "Diagnosed hypertension",
    "Complete-case unadjusted",
    "Race × time"
  ),
  
  extract_global_test(
    hypertension_cc_adj1,
    ~time_2yr:race_ethnicity,
    "Diagnosed hypertension",
    "Complete-case age + sex adjusted",
    "Race × time"
  ),
  
  extract_global_test(
    hypertension_cc_adj2,
    ~time_2yr:race_ethnicity,
    "Diagnosed hypertension",
    "Complete-case fully adjusted",
    "Race × time"
  )
)

hypertension_sensitivity

# 61. Final model sample-size table

adjusted_model_sample_sizes <- adjusted_model_sample_sizes %>%
  mutate(
    model2_retained_pct =
      100 * model2_n / model1_n,
    
    model2_sample_loss_pct =
      100 * (model1_n - model2_n) / model1_n
  )


# 62. Export the reliability tables

binary_reliability_export <- binary_reliability

hba1c_reliability_export <- hba1c_reliability


# 63. Export final analysis tables

dir.create(
  "outputs",
  showWarnings = FALSE
)


write_csv(
  overall_estimates,
  file.path(
    "outputs",
    "overall_weighted_estimates.csv"
  )
)


write_csv(
  race_estimates,
  file.path(
    "outputs",
    "race_ethnicity_weighted_estimates.csv"
  )
)


write_csv(
  trend_model_summary,
  file.path(
    "outputs",
    "trend_model_summary.csv"
  )
)


write_csv(
  global_race_tests,
  file.path(
    "outputs",
    "global_race_tests.csv"
  )
)


write_csv(
  interaction_test_summary,
  file.path(
    "outputs",
    "race_time_interaction_tests.csv"
  )
)


write_csv(
  adjusted_model_sample_sizes,
  file.path(
    "outputs",
    "adjusted_model_sample_sizes.csv"
  )
)


write_csv(
  hypertension_sensitivity,
  file.path(
    "outputs",
    "hypertension_interaction_sensitivity.csv"
  )
)


write_csv(
  binary_reliability_export,
  file.path(
    "outputs",
    "binary_subgroup_reliability.csv"
  )
)


write_csv(
  hba1c_reliability_export,
  file.path(
    "outputs",
    "hba1c_subgroup_reliability.csv"
  )
)


write_csv(
  weight_qa,
  file.path(
    "outputs",
    "survey_weight_qa.csv"
  )
)

# 64. Final export QA

stopifnot(
  nrow(overall_estimates) == 24,
  nrow(race_estimates) == 144,
  nrow(trend_model_summary) == 18,
  nrow(global_race_tests) == 12,
  nrow(interaction_test_summary) == 12,
  nrow(hypertension_sensitivity) == 3
)


list.files(
  "outputs",
  pattern = "\\.csv$"
)
