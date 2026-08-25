# Fixed Stage 1 and Stage 2 model specification —-

spot_stage1_predictors <- c(
  "age.months", "sex", "adm.recent", "wfaz", "cidysymp",
  "not.alert", "hr.all", "rr.all", "envhtemp", "crt.long"
)

spot_stage1_features <- c(
  spot_stage1_predictors,
  "na_ind_adm.recent", "na_ind_hr.all", "na_ind_rr.all", "na_ind_envhtemp"
)

spot_stage1_parameters <- list(
  trees = 5000L,
  eta = 0.00976335122643042,
  max_depth = 160L,
  min_child_weight = 36L,
  max_leaves = 219L,
  gamma = 1.23936294311778e-08,
  lambda = 2.50344369232762e-07,
  alpha = 2.53274958638464e-06,
  base_score = 0.0356865787432118,
  tree_method = "hist",
  grow_policy = "lossguide",
  nthread = 1L,
  imputation_trees = 25L,
  imputation_seed = 6709L
)

spot_stage2_ids <- c(
  "spo2",
  "strem1",
  "crp_strem1",
  "crp_strem1_glucose",
  "spo2_strem1",
  "spo2_strem1_crp",
  "spo2_strem1_crp_glucose"
)

spot_stage2_severe_amber_weight <- function() 5

# Traffic-light classification —-

spot_zone <- function(probability) {
  ifelse(
    probability < 0.005,
    "GREEN",
    ifelse(probability <= 0.02, "AMBER", "RED")
  )
}

# Numerically stable probability transform —-

spot_clip_probability <- function(probability, epsilon = 1e-6) {
  pmin(pmax(probability, epsilon), 1 - epsilon)
}
