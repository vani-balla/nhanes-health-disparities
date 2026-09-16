# ============================================================
# MACHINE-LEARNING EXTENSION
# Predicting prevalent obesity among U.S. adults
# ============================================================


# ----------------------------
# 1. Load packages
# ----------------------------

library(DBI)
library(RPostgres)
library(dplyr)
library(tibble)
library(rstudioapi)

library(tidymodels)
library(glmnet)
library(ranger)

library(readr)

# ----------------------------
# 2. PostgreSQL connection settings
# ----------------------------

db_host <- "localhost"
db_port <- 5432
db_name <- "nhanes_health_disparities"
db_user <- "postgres"

db_password <- askForPassword(
  "Enter your PostgreSQL password"
)


# ----------------------------
# 3. Connect to PostgreSQL
# ----------------------------

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


# ----------------------------
# 4. Import analysis-ready table
# ----------------------------

ml_source <- dbGetQuery(
  con,
  "
  SELECT *
  FROM analytics.analysis_ready
  ORDER BY cycle, participant_id;
  "
)


# ----------------------------
# 5. Disconnect from PostgreSQL
# ----------------------------

dbDisconnect(con)

cat(
  "PostgreSQL connection closed.\n"
)

# ----------------------------
# 6. Confirm imported data
# ----------------------------

tibble(
  total_rows = nrow(ml_source),
  
  unique_participants =
    n_distinct(ml_source$participant_key),
  
  duplicate_participants =
    nrow(ml_source) -
    n_distinct(ml_source$participant_key)
)

# ----------------------------
# 7. Construct ML analytic cohort
# ----------------------------

ml_df <- ml_source %>%
  filter(
    adult_eligible == 1,
    obesity_eligible == 1,
    !is.na(obesity),
    mec_weight > 0
  ) %>%
  select(
    participant_key,
    
    # Prediction target
    obesity,
    
    # Used for temporal train/test split
    cycle,
    
    # Demographic predictors
    age,
    sex,
    
    # Socioeconomic predictors
    education_group,
    poverty_ratio,
    
    # Healthcare-access predictors
    insured,
    has_usual_care,
    
    # Cardiometabolic predictors
    diabetes_dx,
    hypertension_dx,
    
    # Used later for subgroup performance auditing
    race_ethnicity,
    
    # Survey/case weight
    mec_weight
  )

# ----------------------------
# 8. ML cohort QA
# ----------------------------

ml_cohort_qa <- tibble(
  n = nrow(ml_df),
  
  unique_participants =
    n_distinct(ml_df$participant_key),
  
  duplicate_participants =
    nrow(ml_df) -
    n_distinct(ml_df$participant_key),
  
  obesity_cases =
    sum(ml_df$obesity == 1),
  
  obesity_noncases =
    sum(ml_df$obesity == 0),
  
  obesity_missing =
    sum(is.na(ml_df$obesity))
)

ml_cohort_qa

# ----------------------------
# 9. ML cohort by survey period
# ----------------------------

ml_df %>%
  count(cycle) %>%
  arrange(cycle)

# ----------------------------
# 10. Obesity class balance
# ----------------------------

ml_df %>%
  count(obesity) %>%
  mutate(
    percent = 100 * n / sum(n)
  )

# ----------------------------
# 11. Predictor missingness
# ----------------------------

ml_missingness <- tibble(
  variable = c(
    "age",
    "sex",
    "education_group",
    "poverty_ratio",
    "insured",
    "has_usual_care",
    "diabetes_dx",
    "hypertension_dx",
    "race_ethnicity"
  ),
  
  n_missing = c(
    sum(is.na(ml_df$age)),
    sum(is.na(ml_df$sex)),
    sum(is.na(ml_df$education_group)),
    sum(is.na(ml_df$poverty_ratio)),
    sum(is.na(ml_df$insured)),
    sum(is.na(ml_df$has_usual_care)),
    sum(is.na(ml_df$diabetes_dx)),
    sum(is.na(ml_df$hypertension_dx)),
    sum(is.na(ml_df$race_ethnicity))
  )
) %>%
  mutate(
    percent_missing =
      100 * n_missing / nrow(ml_df)
  )

ml_missingness

# TEMPORAL TRAIN / TEST SPLIT

# ----------------------------
# 12. Split earlier vs newer NHANES periods
# ----------------------------

train_df <- ml_df %>%
  filter(
    cycle %in% c(
      "2013-2014",
      "2015-2016",
      "2017-2020"
    )
  )


test_df <- ml_df %>%
  filter(
    cycle == "2021-2023"
  )

# ----------------------------
# 13. Temporal split QA
# ----------------------------

temporal_split_qa <- tibble(
  dataset = c(
    "Training",
    "Test"
  ),
  
  n = c(
    nrow(train_df),
    nrow(test_df)
  ),
  
  obesity_cases = c(
    sum(train_df$obesity == 1),
    sum(test_df$obesity == 1)
  ),
  
  obesity_noncases = c(
    sum(train_df$obesity == 0),
    sum(test_df$obesity == 0)
  ),
  
  obesity_prevalence_pct = c(
    mean(train_df$obesity == 1) * 100,
    mean(test_df$obesity == 1) * 100
  )
)

temporal_split_qa

# ----------------------------
# 14. Check participant separation
# ----------------------------

participant_overlap <- intersect(
  train_df$participant_key,
  test_df$participant_key
)

length(participant_overlap)

unique(train_df$cycle)
unique(test_df$cycle)

# ----------------------------
# 15. Training/test predictor missingness
# ----------------------------

check_ml_missingness <- function(data, dataset_name) {
  
  tibble(
    dataset = dataset_name,
    
    variable = c(
      "age",
      "sex",
      "education_group",
      "poverty_ratio",
      "insured",
      "has_usual_care",
      "diabetes_dx",
      "hypertension_dx"
    ),
    
    n_missing = c(
      sum(is.na(data$age)),
      sum(is.na(data$sex)),
      sum(is.na(data$education_group)),
      sum(is.na(data$poverty_ratio)),
      sum(is.na(data$insured)),
      sum(is.na(data$has_usual_care)),
      sum(is.na(data$diabetes_dx)),
      sum(is.na(data$hypertension_dx))
    )
  ) %>%
    mutate(
      percent_missing =
        100 * n_missing / nrow(data)
    )
}


train_missingness <- check_ml_missingness(
  train_df,
  "Training"
)

test_missingness <- check_ml_missingness(
  test_df,
  "Test"
)

ml_split_missingness <- bind_rows(
  train_missingness,
  test_missingness
)

# ML PREPROCESSING

# ----------------------------
# 16. Prepare modeling variable types
# ----------------------------

recode_obesity_outcome <- function(x) {
  factor(
    case_when(
      as.character(x) %in% c("1", "Yes") ~ "Yes",
      as.character(x) %in% c("0", "No") ~ "No",
      TRUE ~ NA_character_
    ),
    levels = c(
      "Yes",
      "No"
    )
  )
}

train_df <- train_df %>%
  mutate(
    obesity = recode_obesity_outcome(obesity),
    sex = factor(sex),
    education_group = factor(education_group),
    insured = factor(insured),
    has_usual_care = factor(has_usual_care),
    diabetes_dx = factor(diabetes_dx),
    hypertension_dx = factor(hypertension_dx),
    race_ethnicity = factor(race_ethnicity)
  )

test_df <- test_df %>%
  mutate(
    obesity = recode_obesity_outcome(obesity),
    sex = factor(sex),
    education_group = factor(education_group),
    insured = factor(insured),
    has_usual_care = factor(has_usual_care),
    diabetes_dx = factor(diabetes_dx),
    hypertension_dx = factor(hypertension_dx),
    race_ethnicity = factor(race_ethnicity)
  )

# ----------------------------
# 16A. Verify outcome recoding
# ----------------------------

table(train_df$obesity)
table(test_df$obesity)

stopifnot(
  sum(train_df$obesity == "Yes") == 7754,
  sum(train_df$obesity == "No") == 11333,
  sum(test_df$obesity == "Yes") == 2439,
  sum(test_df$obesity == "No") == 3490
)

# ----------------------------
# 17. Training-only cross-validation
# ----------------------------

set.seed(2026)

cv_folds <- vfold_cv(
  train_df,
  v = 5,
  strata = obesity
)

cv_folds

# ----------------------------
# 18. Define preprocessing recipe
# ----------------------------

ml_recipe <- recipe(
  obesity ~
    age +
    sex +
    education_group +
    poverty_ratio +
    insured +
    has_usual_care +
    diabetes_dx +
    hypertension_dx,
  data = train_df
) %>%
  
  # Median imputation for numeric predictor
  step_impute_median(
    poverty_ratio
  ) %>%
  
  # Most-common-category imputation
  step_impute_mode(
    education_group,
    insured,
    has_usual_care,
    diabetes_dx,
    hypertension_dx
  ) %>%
  
  # Convert categorical predictors into dummy variables
  step_dummy(
    all_nominal_predictors()
  )

# ----------------------------
# 19. QA preprocessing recipe
# ----------------------------

recipe_check <- prep(
  ml_recipe,
  training = train_df
)

processed_train_check <- bake(
  recipe_check,
  new_data = NULL
)

glimpse(
  processed_train_check
)

processed_train_check %>%
  summarise(
    across(
      everything(),
      ~sum(is.na(.))
    )
  )

# ============================================================
# BASELINE LOGISTIC REGRESSION
# ============================================================

# ----------------------------
# 20. Define evaluation metrics
# ----------------------------

ml_metrics <- metric_set(
  roc_auc,
  pr_auc,
  accuracy,
  sens,
  yardstick::spec,
  precision,
  recall,
  f_meas,
  mn_log_loss,
  brier_class
)

# ----------------------------
# 21. Baseline logistic-regression specification
# ----------------------------

logistic_spec <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

# ----------------------------
# 22. Baseline logistic workflow
# ----------------------------

logistic_workflow <- workflow() %>%
  add_recipe(
    ml_recipe
  ) %>%
  add_model(
    logistic_spec
  )

logistic_workflow

# ----------------------------
# 23. Cross-validate baseline logistic regression
# ----------------------------

set.seed(2026)

logistic_cv <- fit_resamples(
  logistic_workflow,
  resamples = cv_folds,
  metrics = ml_metrics,
  control = control_resamples(
    save_pred = TRUE
  )
)

# ----------------------------
# 24. View cross-validation performance
# ----------------------------

logistic_cv_metrics <- collect_metrics(
  logistic_cv
)

logistic_cv_metrics

# ----------------------------
# 25. Fold-specific ROC-AUC
# ----------------------------

logistic_fold_auc <- collect_predictions(
  logistic_cv
) %>%
  group_by(id) %>%
  roc_auc(
    truth = obesity,
    .pred_Yes
  )

logistic_fold_auc

# ELASTIC NET LOGISTIC REGRESSION

# 26. Define elastic-net model

elastic_spec <- logistic_reg(
  penalty = tune(),
  mixture = tune()
) %>%
  set_engine("glmnet") %>%
  set_mode("classification")

# ----------------------------
# 27. Elastic-net workflow
# ----------------------------

elastic_workflow <- workflow() %>%
  add_recipe(
    ml_recipe
  ) %>%
  add_model(
    elastic_spec
  )

elastic_workflow

# ----------------------------
# 28. Elastic-net tuning grid
# ----------------------------

elastic_grid <- grid_regular(
  penalty(
    range = c(-4, 0)
  ),
  mixture(),
  levels = c(
    penalty = 20,
    mixture = 6
  )
)

elastic_grid

# ----------------------------
# 29. Tune elastic net
# ----------------------------

set.seed(2026)

elastic_tuning <- tune_grid(
  elastic_workflow,
  resamples = cv_folds,
  grid = elastic_grid,
  metrics = ml_metrics,
  control = control_grid(
    save_pred = TRUE
  )
)

# ----------------------------
# 30. Best elastic-net candidates
# ----------------------------

show_best(
  elastic_tuning,
  metric = "roc_auc",
  n = 10
)

best_elastic <- select_best(
  elastic_tuning,
  metric = "roc_auc"
)

best_elastic

# ----------------------------
# 31. Finalize elastic-net workflow
# ----------------------------

final_elastic_workflow <- finalize_workflow(
  elastic_workflow,
  best_elastic
)

final_elastic_workflow

# ============================================================
# RANDOM FOREST
# ============================================================

# ----------------------------
# 32. Define random-forest model
# ----------------------------

rf_spec <- rand_forest(
  trees = 500,
  mtry = tune(),
  min_n = tune()
) %>%
  set_engine(
    "ranger",
    importance = "permutation"
  ) %>%
  set_mode("classification")

# ----------------------------
# 33. Random-forest workflow
# ----------------------------

rf_workflow <- workflow() %>%
  add_recipe(
    ml_recipe
  ) %>%
  add_model(
    rf_spec
  )

rf_workflow

# ----------------------------
# 34. Random-forest tuning grid
# ----------------------------

set.seed(2026)

rf_grid <- grid_latin_hypercube(
  mtry(
    range = c(1L, 10L)
  ),
  min_n(
    range = c(2L, 40L)
  ),
  size = 25
)

rf_grid

rf_tuning_metrics <- metric_set(
  roc_auc,
  pr_auc
)

# ----------------------------
# 35. Tune random forest
# ----------------------------

set.seed(2026)

rf_tuning <- tune_grid(
  rf_workflow,
  resamples = cv_folds,
  grid = rf_grid,
  metrics = rf_tuning_metrics,
  control = control_grid(
    save_pred = TRUE
  )
)

# ----------------------------
# 36. Best random-forest candidates
# ----------------------------

show_best(
  rf_tuning,
  metric = "roc_auc",
  n = 10
)

best_rf <- select_best(
  rf_tuning,
  metric = "roc_auc"
)

best_rf

# ----------------------------
# 37. Finalize random-forest workflow
# ----------------------------

final_rf_workflow <- finalize_workflow(
  rf_workflow,
  best_rf
)

final_rf_workflow

# ============================================================
# FINAL TEMPORAL TEST-SET EVALUATION
# ============================================================

# ----------------------------
# 38. Fit final models on full training data
# ----------------------------

final_logistic_fit <- fit(
  logistic_workflow,
  data = train_df
)

final_elastic_fit <- fit(
  final_elastic_workflow,
  data = train_df
)

set.seed(2026)

final_rf_fit <- fit(
  final_rf_workflow,
  data = train_df
)

# ----------------------------
# 39. Predict held-out 2021–2023 test set
# ----------------------------

logistic_test_predictions <- bind_cols(
  test_df %>%
    select(
      participant_key,
      obesity,
      race_ethnicity,
      mec_weight
    ),
  
  predict(
    final_logistic_fit,
    new_data = test_df,
    type = "prob"
  ),
  
  predict(
    final_logistic_fit,
    new_data = test_df,
    type = "class"
  )
)


elastic_test_predictions <- bind_cols(
  test_df %>%
    select(
      participant_key,
      obesity,
      race_ethnicity,
      mec_weight
    ),
  
  predict(
    final_elastic_fit,
    new_data = test_df,
    type = "prob"
  ),
  
  predict(
    final_elastic_fit,
    new_data = test_df,
    type = "class"
  )
)


rf_test_predictions <- bind_cols(
  test_df %>%
    select(
      participant_key,
      obesity,
      race_ethnicity,
      mec_weight
    ),
  
  predict(
    final_rf_fit,
    new_data = test_df,
    type = "prob"
  ),
  
  predict(
    final_rf_fit,
    new_data = test_df,
    type = "class"
  )
)

# ----------------------------
# 40. Test prediction QA
# ----------------------------

prediction_qa <- tibble(
  model = c(
    "Logistic regression",
    "Elastic net",
    "Random forest"
  ),
  
  n_predictions = c(
    nrow(logistic_test_predictions),
    nrow(elastic_test_predictions),
    nrow(rf_test_predictions)
  ),
  
  missing_probabilities = c(
    sum(is.na(logistic_test_predictions$.pred_Yes)),
    sum(is.na(elastic_test_predictions$.pred_Yes)),
    sum(is.na(rf_test_predictions$.pred_Yes))
  ),
  
  min_probability = c(
    min(logistic_test_predictions$.pred_Yes),
    min(elastic_test_predictions$.pred_Yes),
    min(rf_test_predictions$.pred_Yes)
  ),
  
  max_probability = c(
    max(logistic_test_predictions$.pred_Yes),
    max(elastic_test_predictions$.pred_Yes),
    max(rf_test_predictions$.pred_Yes)
  )
)

prediction_qa

# ----------------------------
# 41. Held-out test-set performance
# ----------------------------

test_metrics <- metric_set(
  roc_auc,
  pr_auc,
  accuracy,
  sens,
  yardstick::spec,
  precision,
  recall,
  f_meas,
  mn_log_loss,
  brier_class
)

logistic_test_metrics <- test_metrics(
  logistic_test_predictions,
  truth = obesity,
  estimate = .pred_class,
  .pred_Yes
) %>%
  mutate(
    model = "Logistic regression"
  )


elastic_test_metrics <- test_metrics(
  elastic_test_predictions,
  truth = obesity,
  estimate = .pred_class,
  .pred_Yes
) %>%
  mutate(
    model = "Elastic net"
  )


rf_test_metrics <- test_metrics(
  rf_test_predictions,
  truth = obesity,
  estimate = .pred_class,
  .pred_Yes
) %>%
  mutate(
    model = "Random forest"
  )

model_test_comparison <- bind_rows(
  logistic_test_metrics,
  elastic_test_metrics,
  rf_test_metrics
) %>%
  select(
    model,
    .metric,
    .estimator,
    .estimate
  )

model_test_comparison

# ----------------------------
# 42. Test performance comparison
# ----------------------------

model_test_comparison_wide <- model_test_comparison %>%
  select(
    model,
    .metric,
    .estimate
  ) %>%
  tidyr::pivot_wider(
    names_from = .metric,
    values_from = .estimate
  )

model_test_comparison_wide

# ============================================================
# MODEL CALIBRATION
# ============================================================

# ----------------------------
# 43. Create calibration data
# ----------------------------

logistic_calibration <- logistic_test_predictions %>%
  mutate(
    model = "Logistic regression",
    obesity_numeric =
      if_else(obesity == "Yes", 1, 0),
    probability_bin =
      ntile(.pred_Yes, 10)
  ) %>%
  group_by(
    model,
    probability_bin
  ) %>%
  summarise(
    n = n(),
    
    mean_predicted_probability =
      mean(.pred_Yes),
    
    observed_obesity_rate =
      mean(obesity_numeric),
    
    .groups = "drop"
  )


rf_calibration <- rf_test_predictions %>%
  mutate(
    model = "Random forest",
    obesity_numeric =
      if_else(obesity == "Yes", 1, 0),
    probability_bin =
      ntile(.pred_Yes, 10)
  ) %>%
  group_by(
    model,
    probability_bin
  ) %>%
  summarise(
    n = n(),
    
    mean_predicted_probability =
      mean(.pred_Yes),
    
    observed_obesity_rate =
      mean(obesity_numeric),
    
    .groups = "drop"
  )


calibration_data <- bind_rows(
  logistic_calibration,
  rf_calibration
)

calibration_data

# ----------------------------
# 44. Calibration intercept and slope
# ----------------------------

calculate_calibration <- function(prediction_data, model_name) {
  
  calibration_df <- prediction_data %>%
    mutate(
      obesity_numeric =
        if_else(
          obesity == "Yes",
          1,
          0
        ),
      
      predicted_logit =
        qlogis(
          pmin(
            pmax(
              .pred_Yes,
              1e-6
            ),
            1 - 1e-6
          )
        )
    )
  
  
  intercept_model <- glm(
    obesity_numeric ~
      offset(predicted_logit),
    data = calibration_df,
    family = binomial()
  )
  
  
  slope_model <- glm(
    obesity_numeric ~
      predicted_logit,
    data = calibration_df,
    family = binomial()
  )
  
  
  tibble(
    model = model_name,
    
    calibration_intercept =
      as.numeric(
        coef(intercept_model)[1]
      ),
    
    calibration_slope =
      as.numeric(
        coef(slope_model)["predicted_logit"]
      )
  )
}

calibration_summary <- bind_rows(
  
  calculate_calibration(
    logistic_test_predictions,
    "Logistic regression"
  ),
  
  calculate_calibration(
    rf_test_predictions,
    "Random forest"
  )
)

calibration_summary

# ----------------------------
# 45. Calibration plot
# ----------------------------

calibration_plot <- ggplot(
  calibration_data,
  aes(
    x = mean_predicted_probability,
    y = observed_obesity_rate,
    group = model
  )
) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed"
  ) +
  geom_line() +
  geom_point() +
  facet_wrap(
    ~model
  ) +
  labs(
    title =
      "Calibration of Obesity Prediction Models",
    subtitle =
      "Held-out NHANES 2021–2023 test set",
    x =
      "Mean predicted probability of obesity",
    y =
      "Observed proportion with obesity"
  ) +
  theme_minimal()

calibration_plot

# ============================================================
# CLASSIFICATION THRESHOLD
# ============================================================

# ----------------------------
# 46. Extract CV predictions for best random forest
# ----------------------------

rf_cv_predictions <- collect_predictions(
  rf_tuning,
  parameters = best_rf
)

rf_cv_predictions %>%
  count(obesity)

# ----------------------------
# 47. Evaluate classification thresholds
# ----------------------------

threshold_grid <- seq(
  0.20,
  0.70,
  by = 0.01
)


threshold_results <- purrr::map_dfr(
  threshold_grid,
  function(threshold) {
    
    predicted_class <- factor(
      if_else(
        rf_cv_predictions$.pred_Yes >= threshold,
        "Yes",
        "No"
      ),
      levels = c(
        "Yes",
        "No"
      )
    )
    
    tibble(
      threshold = threshold,
      
      sensitivity = sens_vec(
        truth = rf_cv_predictions$obesity,
        estimate = predicted_class
      ),
      
      specificity = spec_vec(
        truth = rf_cv_predictions$obesity,
        estimate = predicted_class
      )
    )
  }
) %>%
  mutate(
    youden_j =
      sensitivity +
      specificity -
      1
  )

threshold_results

# ----------------------------
# 48. Select CV-derived threshold
# ----------------------------

best_threshold <- threshold_results %>%
  slice_max(
    youden_j,
    n = 1,
    with_ties = FALSE
  )

best_threshold

#Step 49 — Compare it with 0.50#

threshold_results %>%
  filter(
    threshold == 0.50 |
      threshold == best_threshold$threshold
  )

# ============================================================
# RANDOM-FOREST THRESHOLD EVALUATION
# ============================================================

# ----------------------------
# 50. Apply CV-selected threshold to test set
# ----------------------------

rf_threshold <- best_threshold$threshold


rf_test_predictions <- rf_test_predictions %>%
  mutate(
    pred_class_cv_threshold = factor(
      if_else(
        .pred_Yes >= rf_threshold,
        "Yes",
        "No"
      ),
      levels = c(
        "Yes",
        "No"
      )
    )
  )

table(
  rf_test_predictions$pred_class_cv_threshold
)

# ----------------------------
# 51. Test performance at CV-selected threshold
# ----------------------------

rf_threshold_test_metrics <- tibble(
  
  threshold = rf_threshold,
  
  sensitivity = sens_vec(
    truth = rf_test_predictions$obesity,
    estimate =
      rf_test_predictions$pred_class_cv_threshold
  ),
  
  specificity = spec_vec(
    truth = rf_test_predictions$obesity,
    estimate =
      rf_test_predictions$pred_class_cv_threshold
  ),
  
  precision = precision_vec(
    truth = rf_test_predictions$obesity,
    estimate =
      rf_test_predictions$pred_class_cv_threshold
  ),
  
  f1 = f_meas_vec(
    truth = rf_test_predictions$obesity,
    estimate =
      rf_test_predictions$pred_class_cv_threshold
  ),
  
  accuracy = accuracy_vec(
    truth = rf_test_predictions$obesity,
    estimate =
      rf_test_predictions$pred_class_cv_threshold
  )
)

rf_threshold_test_metrics

# ============================================================
# RACE/ETHNICITY PERFORMANCE AUDIT
# ============================================================


# ----------------------------
# 52. Subgroup sample sizes
# ----------------------------

race_test_counts <- rf_test_predictions %>%
  count(
    race_ethnicity,
    obesity
  ) %>%
  tidyr::pivot_wider(
    names_from = obesity,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(
    total_n = Yes + No
  )

race_test_counts

# ----------------------------
# 53. Race/ethnicity subgroup performance
# ----------------------------

race_performance <- rf_test_predictions %>%
  group_by(
    race_ethnicity
  ) %>%
  summarise(
    
    n = n(),
    
    obesity_cases =
      sum(obesity == "Yes"),
    
    obesity_noncases =
      sum(obesity == "No"),
    
    obesity_prevalence =
      mean(obesity == "Yes"),
    
    roc_auc =
      roc_auc_vec(
        truth = obesity,
        estimate = .pred_Yes
      ),
    
    pr_auc =
      pr_auc_vec(
        truth = obesity,
        estimate = .pred_Yes
      ),
    
    sensitivity =
      sens_vec(
        truth = obesity,
        estimate =
          pred_class_cv_threshold
      ),
    
    specificity =
      spec_vec(
        truth = obesity,
        estimate =
          pred_class_cv_threshold
      ),
    
    precision =
      precision_vec(
        truth = obesity,
        estimate =
          pred_class_cv_threshold
      ),
    
    f1 =
      f_meas_vec(
        truth = obesity,
        estimate =
          pred_class_cv_threshold
      ),
    
    brier_score =
      mean(
        (
          if_else(
            obesity == "Yes",
            1,
            0
          ) -
            .pred_Yes
        )^2
      ),
    
    .groups = "drop"
  )

race_performance

# ============================================================
# SURVEY-WEIGHTED ML PERFORMANCE SENSITIVITY
# ============================================================

# ----------------------------
# 54. Create test-set case weights
# ----------------------------

rf_test_predictions <- rf_test_predictions %>%
  mutate(
    ml_case_weight =
      hardhat::importance_weights(
        mec_weight
      )
  )

summary(
  rf_test_predictions$mec_weight
)

# ----------------------------
# 55. Weighted ROC-AUC and PR-AUC
# ----------------------------

rf_weighted_discrimination <- tibble(
  
  roc_auc = roc_auc_vec(
    truth =
      rf_test_predictions$obesity,
    estimate =
      rf_test_predictions$.pred_Yes,
    case_weights =
      rf_test_predictions$ml_case_weight
  ),
  
  pr_auc = pr_auc_vec(
    truth =
      rf_test_predictions$obesity,
    estimate =
      rf_test_predictions$.pred_Yes,
    case_weights =
      rf_test_predictions$ml_case_weight
  )
)

rf_weighted_discrimination

# ----------------------------
# 56. Weighted threshold performance
# ----------------------------

weighted_tp <- sum(
  rf_test_predictions$mec_weight[
    rf_test_predictions$obesity == "Yes" &
      rf_test_predictions$pred_class_cv_threshold == "Yes"
  ]
)

weighted_fn <- sum(
  rf_test_predictions$mec_weight[
    rf_test_predictions$obesity == "Yes" &
      rf_test_predictions$pred_class_cv_threshold == "No"
  ]
)

weighted_tn <- sum(
  rf_test_predictions$mec_weight[
    rf_test_predictions$obesity == "No" &
      rf_test_predictions$pred_class_cv_threshold == "No"
  ]
)

weighted_fp <- sum(
  rf_test_predictions$mec_weight[
    rf_test_predictions$obesity == "No" &
      rf_test_predictions$pred_class_cv_threshold == "Yes"
  ]
)


rf_weighted_threshold_metrics <- tibble(
  
  threshold = rf_threshold,
  
  sensitivity =
    weighted_tp /
    (weighted_tp + weighted_fn),
  
  specificity =
    weighted_tn /
    (weighted_tn + weighted_fp),
  
  precision =
    weighted_tp /
    (weighted_tp + weighted_fp),
  
  accuracy =
    (weighted_tp + weighted_tn) /
    (
      weighted_tp +
        weighted_fn +
        weighted_tn +
        weighted_fp
    )
) %>%
  mutate(
    f1 =
      2 *
      precision *
      sensitivity /
      (
        precision +
          sensitivity
      )
  )

rf_weighted_threshold_metrics

# ============================================================
# RANDOM-FOREST FEATURE IMPORTANCE
# ============================================================

# ----------------------------
# 57. Extract fitted random-forest engine
# ----------------------------

rf_engine <- extract_fit_engine(
  final_rf_fit
)

rf_engine

# ----------------------------
# 58. Extract permutation importance
# ----------------------------

rf_feature_importance <- tibble(
  variable =
    names(
      rf_engine$variable.importance
    ),
  
  importance =
    as.numeric(
      rf_engine$variable.importance
    )
) %>%
  arrange(
    desc(importance)
  )

rf_feature_importance

# ----------------------------
# 59. Plot random-forest feature importance
# ----------------------------

rf_importance_plot <- rf_feature_importance %>%
  mutate(
    variable =
      forcats::fct_reorder(
        variable,
        importance
      )
  ) %>%
  ggplot(
    aes(
      x = importance,
      y = variable
    )
  ) +
  geom_col() +
  labs(
    title =
      "Random-Forest Predictor Importance",
    subtitle =
      "Permutation importance from final obesity prediction model",
    x =
      "Permutation importance",
    y =
      "Predictor"
  ) +
  theme_minimal()

rf_importance_plot

# ============================================================
# FINAL ML SUMMARY TABLES
# ============================================================

# ----------------------------
# 60. Cross-validation model comparison
# ----------------------------

logistic_cv_summary <- logistic_cv_metrics %>%
  filter(
    .metric %in% c(
      "roc_auc",
      "pr_auc"
    )
  ) %>%
  transmute(
    model = "Logistic regression",
    metric = .metric,
    mean = mean,
    std_err = std_err
  )


elastic_cv_summary <- collect_metrics(
  elastic_tuning
) %>%
  semi_join(
    best_elastic,
    by = c(
      "penalty",
      "mixture"
    )
  ) %>%
  filter(
    .metric %in% c(
      "roc_auc",
      "pr_auc"
    )
  ) %>%
  transmute(
    model = "Elastic net",
    metric = .metric,
    mean = mean,
    std_err = std_err
  )


rf_cv_summary <- collect_metrics(
  rf_tuning
) %>%
  semi_join(
    best_rf,
    by = c(
      "mtry",
      "min_n"
    )
  ) %>%
  filter(
    .metric %in% c(
      "roc_auc",
      "pr_auc"
    )
  ) %>%
  transmute(
    model = "Random forest",
    metric = .metric,
    mean = mean,
    std_err = std_err
  )


cv_model_comparison <- bind_rows(
  logistic_cv_summary,
  elastic_cv_summary,
  rf_cv_summary
)

cv_model_comparison

# ----------------------------
# 61. Create presentation-ready feature importance
# ----------------------------

rf_feature_importance_clean <- rf_feature_importance %>%
  mutate(
    predictor = case_when(
      variable == "hypertension_dx_X1" ~
        "Diagnosed hypertension",
      
      variable == "diabetes_dx_X1" ~
        "Diagnosed diabetes",
      
      variable == "age" ~
        "Age",
      
      variable == "sex_Male" ~
        "Male sex",
      
      variable ==
        "education_group_Some.college.AA" ~
        "Some college / AA",
      
      variable == "poverty_ratio" ~
        "Poverty-income ratio",
      
      variable == "has_usual_care_X1" ~
        "Usual source of care",
      
      variable == "insured_X1" ~
        "Health insurance",
      
      variable ==
        "education_group_High.school.GED" ~
        "High school / GED",
      
      variable ==
        "education_group_Less.than.high.school" ~
        "Less than high school",
      
      TRUE ~ variable
    )
  ) %>%
  select(
    predictor,
    importance
  )

rf_feature_importance_clean

rf_importance_plot <- rf_feature_importance_clean %>%
  mutate(
    predictor =
      forcats::fct_reorder(
        predictor,
        importance
      )
  ) %>%
  ggplot(
    aes(
      x = importance,
      y = predictor
    )
  ) +
  geom_col() +
  labs(
    title =
      "Random-Forest Predictor Importance",
    subtitle =
      "Permutation importance from final obesity prediction model",
    x =
      "Permutation importance",
    y =
      "Predictor"
  ) +
  theme_minimal()

rf_importance_plot

# ============================================================
# EXPORT ML RESULTS
# ============================================================

# ----------------------------
# 62. Export final ML tables
# ----------------------------

dir.create(
  "outputs",
  showWarnings = FALSE
)


write_csv(
  temporal_split_qa,
  file.path(
    "outputs",
    "ml_temporal_split_qa.csv"
  )
)

write_csv(
  ml_split_missingness,
  file.path(
    "outputs",
    "ml_predictor_missingness.csv"
  )
)

write_csv(
  cv_model_comparison,
  file.path(
    "outputs",
    "ml_cv_model_comparison.csv"
  )
)


write_csv(
  model_test_comparison,
  file.path(
    "outputs",
    "ml_temporal_test_performance.csv"
  )
)


write_csv(
  prediction_qa,
  file.path(
    "outputs",
    "ml_prediction_qa.csv"
  )
)


write_csv(
  calibration_data,
  file.path(
    "outputs",
    "ml_calibration_bins.csv"
  )
)


write_csv(
  calibration_summary,
  file.path(
    "outputs",
    "ml_calibration_summary.csv"
  )
)


write_csv(
  threshold_results,
  file.path(
    "outputs",
    "ml_rf_threshold_training_cv.csv"
  )
)


write_csv(
  rf_threshold_test_metrics,
  file.path(
    "outputs",
    "ml_rf_threshold_test_performance.csv"
  )
)


write_csv(
  race_test_counts,
  file.path(
    "outputs",
    "ml_race_test_counts.csv"
  )
)


write_csv(
  race_performance,
  file.path(
    "outputs",
    "ml_race_performance.csv"
  )
)


write_csv(
  rf_weighted_discrimination,
  file.path(
    "outputs",
    "ml_rf_weighted_discrimination.csv"
  )
)


write_csv(
  rf_weighted_threshold_metrics,
  file.path(
    "outputs",
    "ml_rf_weighted_threshold_performance.csv"
  )
)


write_csv(
  rf_feature_importance_clean,
  file.path(
    "outputs",
    "ml_rf_feature_importance.csv"
  )
)

# ----------------------------
# 63. Save ML figures
# ----------------------------

dir.create(
  file.path(
    "outputs",
    "figures"
  ),
  showWarnings = FALSE
)


ggsave(
  filename =
    file.path(
      "outputs",
      "figures",
      "ml_calibration_plot.png"
    ),
  plot = calibration_plot,
  width = 8,
  height = 5,
  dpi = 300
)


ggsave(
  filename =
    file.path(
      "outputs",
      "figures",
      "ml_rf_feature_importance.png"
    ),
  plot = rf_importance_plot,
  width = 8,
  height = 5,
  dpi = 300
)

# ----------------------------
# 64. Final ML export QA
# ----------------------------

stopifnot(
  
  # Cohort
  nrow(train_df) == 19087,
  nrow(test_df) == 5929,
  
  # Outcome
  sum(train_df$obesity == "Yes") == 7754,
  sum(test_df$obesity == "Yes") == 2439,
  
  # No temporal overlap
  length(
    intersect(
      train_df$participant_key,
      test_df$participant_key
    )
  ) == 0,
  
  # Model summaries
  nrow(cv_model_comparison) == 6,
  nrow(model_test_comparison) == 30,
  
  # Calibration
  nrow(calibration_data) == 20,
  nrow(calibration_summary) == 2,
  
  # Threshold
  nrow(best_threshold) == 1,
  dplyr::near(
    best_threshold$threshold,
    0.41
  ),
  
  # Race audit
  nrow(race_performance) == 6,
  
  # Feature importance
  nrow(rf_feature_importance_clean) == 10
)


list.files(
  "outputs",
  pattern = "^ml_.*\\.csv$"
)


list.files(
  file.path(
    "outputs",
    "figures"
  )
)