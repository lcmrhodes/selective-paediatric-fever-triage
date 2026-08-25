#!/usr/bin/env Rscript

# Dependencies and command-line contract —-

suppressPackageStartupMessages({
  library(jsonlite)
  library(recipes)
  library(workflows)
  library(xgboost)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 12L) {
  stop(
    paste(
      "Usage: Rscript tools/build_model_bundle.R",
      "<stage1-workflow> <stage1-calibrator> <stage2-metadata>",
      "<spo2> <strem1> <crp-strem1> <crp-strem1-glucose>",
      "<spo2-strem1> <spo2-strem1-crp> <spo2-strem1-crp-glucose>",
      "<verification-data> <output-directory>"
    ),
    call. = FALSE
  )
}

names(args) <- c(
  "stage1_workflow", "stage1_calibrator", "stage2_metadata",
  "spo2", "strem1", "crp_strem1", "crp_strem1_glucose",
  "spo2_strem1", "spo2_strem1_crp", "spo2_strem1_crp_glucose",
  "verification_data", "output"
)

# Participant-level object stripping —-

strip_tree <- function(tree) {
  tree$bindx <- integer()
  tree$btree$where <- integer()
  tree$btree$y <- NULL
  tree$btree$na.action <- NULL
  tree$btree$residuals <- NULL
  tree$btree$model <- NULL
  tree$btree$x <- NULL
  tree$btree$weights <- NULL
  attr(tree$btree$terms, ".Environment") <- baseenv()
  tree
}

strip_imputer <- function(model) {
  model$y <- model$y[0]
  model$X <- NULL
  model$OOB <- NULL
  model$mtrees <- lapply(model$mtrees, strip_tree)
  model
}

workflow <- readRDS(args[["stage1_workflow"]])
preprocessor <- workflows::extract_recipe(workflow, estimated = TRUE)
imputation_step <- which(vapply(preprocessor$steps, inherits, logical(1), "step_impute_bag"))
if (length(imputation_step) != 1L) stop("Unexpected Stage 1 preprocessing contract.", call. = FALSE)
preprocessor$steps[[imputation_step]]$models <- lapply(
  preprocessor$steps[[imputation_step]]$models,
  strip_imputer
)
preprocessor$orig_lvls[c("label", "site", "ipdopd", "sampling_weight", "outcome.binary")] <- NULL
preprocessor <- recipes::update_role_requirements(preprocessor, role = "id", bake = FALSE)

find_long_vectors <- function(object, path = "preprocessor", limit = 1000L) {
  if (is.atomic(object)) {
    if (length(object) >= limit) return(paste0(path, " [", length(object), "]"))
    return(character())
  }
  if (is.environment(object) || is.function(object) || is.null(object)) return(character())
  if (is.pairlist(object)) object <- as.list(object)
  if (!is.list(object)) return(character())
  child_names <- names(object)
  if (is.null(child_names)) child_names <- as.character(seq_along(object))
  unlist(lapply(seq_along(object), function(i) {
    find_long_vectors(object[[i]], paste0(path, "$", child_names[[i]]), limit)
  }), use.names = FALSE)
}
long_vectors <- find_long_vectors(preprocessor)
if (length(long_vectors)) {
  stop(
    "The stripped Stage 1 preprocessor still contains row-length vector(s): ",
    paste(long_vectors, collapse = ", "),
    call. = FALSE
  )
}

# Minimal Stage 1 artifact export —-

dir.create(args[["output"]], recursive = TRUE, showWarnings = FALSE)
preprocessor_path <- file.path(args[["output"]], "stage1_preprocessor.rds")
booster_path <- file.path(args[["output"]], "stage1_booster.ubj")
calibration_path <- file.path(args[["output"]], "stage1_calibration.json")
stage2_path <- file.path(args[["output"]], "stage2_models.json")
manifest_path <- file.path(args[["output"]], "model_bundle_manifest.json")

saveRDS(preprocessor, preprocessor_path, version = 3, compress = "xz")
booster <- workflows::extract_fit_engine(workflow)
invisible(xgboost::xgb.save(booster, booster_path))

calibrator <- readRDS(args[["stage1_calibrator"]])
calibration <- list(
  method = "weighted Platt scaling",
  intercept = unname(calibrator$coefficients[["intercept"]]),
  slope = unname(calibrator$coefficients[["slope"]]),
  clip_epsilon = calibrator$raw_clip_epsilon
)
jsonlite::write_json(calibration, calibration_path, auto_unbox = TRUE, pretty = TRUE, digits = 17)

# Minimal Stage 2 parameter export —-

metadata <- readRDS(args[["stage2_metadata"]])
medians <- as.list(metadata$stage2_medians)
names(medians) <- c("spo2", "strem1", "crp", "glucose")

stage2_labels <- c(
  spo2 = "SpO2",
  strem1 = "sTREM-1",
  crp_strem1 = "CRP + sTREM-1",
  crp_strem1_glucose = "CRP + sTREM-1 + glucose",
  spo2_strem1 = "SpO2 + sTREM-1",
  spo2_strem1_crp = "SpO2 + sTREM-1 + CRP",
  spo2_strem1_crp_glucose = "SpO2 + sTREM-1 + CRP + glucose"
)
predictor_names <- list(
  spo2 = "spo2",
  strem1 = "strem1",
  crp_strem1 = c("crp", "strem1"),
  crp_strem1_glucose = c("crp", "strem1", "glucose"),
  spo2_strem1 = c("spo2", "strem1"),
  spo2_strem1_crp = c("spo2", "strem1", "crp"),
  spo2_strem1_crp_glucose = c("spo2", "strem1", "crp", "glucose")
)
source_terms <- list(
  spo2 = "oxy.ra",
  strem1 = "sTREM1",
  crp_strem1 = c("CRP", "sTREM1"),
  crp_strem1_glucose = c("CRP", "sTREM1", "glucose"),
  spo2_strem1 = c("oxy.ra", "sTREM1"),
  spo2_strem1_crp = c("oxy.ra", "sTREM1", "CRP"),
  spo2_strem1_crp_glucose = c("oxy.ra", "sTREM1", "CRP", "glucose")
)

strategies <- lapply(names(stage2_labels), function(id) {
  fit <- readRDS(args[[id]])
  coefficient <- stats::coef(fit)
  values <- as.list(unname(coefficient[-1]))
  names(values) <- predictor_names[[id]]
  list(
    label = unname(stage2_labels[[id]]),
    predictors = unname(predictor_names[[id]]),
    intercept = unname(coefficient[[1]]),
    coefficients = values
  )
})
names(strategies) <- names(stage2_labels)
stage2 <- list(
  offset_coefficient = 1,
  medians = medians,
  strategies = strategies
)
jsonlite::write_json(stage2, stage2_path, auto_unbox = TRUE, pretty = TRUE, digits = 17)

# Prediction-equivalence verification —-

verification <- readRDS(args[["verification_data"]])
original_probability <- predict(workflow, verification, type = "prob")$.pred_1
stage1_predictors <- c(
  "age.months", "sex", "adm.recent", "wfaz", "cidysymp",
  "not.alert", "hr.all", "rr.all", "envhtemp", "crt.long"
)
safe_input <- as.data.frame(verification[, stage1_predictors, drop = FALSE])
for (name in stage1_predictors) safe_input[[name]] <- as.numeric(safe_input[[name]])
baked <- recipes::bake(preprocessor, new_data = safe_input)
features <- booster$feature_names
matrix <- as.matrix(baked[, features, drop = FALSE])
storage.mode(matrix) <- "double"
sparse_matrix <- Matrix::Matrix(matrix, sparse = TRUE)
stripped_probability <- 1 - as.numeric(
  predict(
    xgboost::xgb.load(booster_path),
    xgboost::xgb.DMatrix(sparse_matrix, missing = NA)
  )
)
stage1_max_difference <- max(abs(original_probability - stripped_probability))
if (!is.finite(stage1_max_difference) || stage1_max_difference > 1e-15) {
  stop(
    "Stripped Stage 1 prediction parity failed; maximum absolute difference = ",
    format(stage1_max_difference, digits = 17),
    call. = FALSE
  )
}

verification$s1_logit <- 0
verification$sTREM1 <- verification$STREM1
verification$glucose <- verification$lbglu
for (name in names(medians)) {
  source_name <- c(spo2 = "oxy.ra", strem1 = "sTREM1", crp = "CRP", glucose = "glucose")[[name]]
  verification[[source_name]][is.na(verification[[source_name]])] <- medians[[name]]
}
stage2_max_difference <- 0
for (id in names(stage2_labels)) {
  fit <- readRDS(args[[id]])
  original <- as.numeric(predict(fit, newdata = verification, type = "response"))
  coefficient <- stats::coef(fit)
  linear <- coefficient[[1]] + verification$s1_logit
  for (term in source_terms[[id]]) linear <- linear + coefficient[[term]] * verification[[term]]
  stage2_max_difference <- max(stage2_max_difference, abs(original - stats::plogis(linear)))
}
if (!is.finite(stage2_max_difference) || stage2_max_difference > 1e-14) {
  stop("Stage 2 coefficient parity failed.", call. = FALSE)
}

artifacts <- c(preprocessor_path, booster_path, calibration_path, stage2_path)

# Public distribution manifest —-

manifest <- list(
  publication_model_date = "2026-08-24",
  participant_rows_included = FALSE,
  files = lapply(artifacts, function(path) {
    list(file = basename(path), bytes = unname(file.info(path)$size))
  }),
  verification = list(
    stage1_max_absolute_probability_difference = stage1_max_difference,
    stage2_max_absolute_probability_difference = stage2_max_difference,
    verification_rows = nrow(verification)
  )
)
jsonlite::write_json(manifest, manifest_path, auto_unbox = TRUE, pretty = TRUE, digits = 17)

message("Wrote public-safe prediction bundle to ", normalizePath(args[["output"]]))
