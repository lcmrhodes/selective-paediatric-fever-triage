# Model-bundle discovery and loading —-

spot_project_root <- function() {
  root <- Sys.getenv("SPOT_SEPSIS_ROOT", unset = getwd())
  normalizePath(root, mustWork = TRUE)
}

spot_model_paths <- function(root = spot_project_root()) {
  list(
    preprocessor = file.path(root, "models", "stage1_preprocessor.rds"),
    booster = file.path(root, "models", "stage1_booster.ubj"),
    calibration = file.path(root, "models", "stage1_calibration.json"),
    stage2 = file.path(root, "models", "stage2_models.json")
  )
}

spot_load_models <- function(root = spot_project_root()) {
  paths <- spot_model_paths(root)
  missing <- unlist(paths)[!file.exists(unlist(paths))]
  if (length(missing)) {
    stop("Prediction model file missing: ", paste(basename(missing), collapse = ", "), call. = FALSE)
  }

  list(
    preprocessor = readRDS(paths$preprocessor),
    booster = xgboost::xgb.load(paths$booster),
    calibration = jsonlite::read_json(paths$calibration, simplifyVector = TRUE),
    stage2 = jsonlite::read_json(paths$stage2, simplifyVector = FALSE)
  )
}

# Stage 1 input contract —-

spot_validate_stage1_input <- function(data) {
  missing_columns <- setdiff(spot_stage1_predictors, names(data))
  if (length(missing_columns)) {
    stop("Stage 1 input is missing field(s): ", paste(missing_columns, collapse = ", "), call. = FALSE)
  }
  out <- as.data.frame(data[, spot_stage1_predictors, drop = FALSE])
  for (name in spot_stage1_predictors) out[[name]] <- as.numeric(out[[name]])

  binary <- c("sex", "adm.recent", "not.alert", "crt.long")
  bad_binary <- vapply(binary, function(name) {
    any(!is.na(out[[name]]) & !out[[name]] %in% c(0, 1))
  }, logical(1))
  if (any(bad_binary)) {
    stop("Binary Stage 1 fields must be 0, 1, or missing: ", paste(binary[bad_binary], collapse = ", "), call. = FALSE)
  }
  out
}

# Stage 1 prediction and calibration —-

spot_predict_stage1 <- function(data, models = spot_load_models()) {
  raw <- spot_validate_stage1_input(data)
  baked <- recipes::bake(models$preprocessor, new_data = raw)
  missing_features <- setdiff(spot_stage1_features, names(baked))
  if (length(missing_features)) {
    stop("Stage 1 preprocessing did not produce the fitted feature contract.", call. = FALSE)
  }

  matrix <- as.matrix(baked[, spot_stage1_features, drop = FALSE])
  storage.mode(matrix) <- "double"
  sparse_matrix <- Matrix::Matrix(matrix, sparse = TRUE)
  engine_probability <- as.numeric(
    predict(models$booster, xgboost::xgb.DMatrix(sparse_matrix, missing = NA))
  )
  raw_probability <- 1 - engine_probability
  calibrated_probability <- stats::plogis(
    models$calibration$intercept +
      models$calibration$slope * stats::qlogis(
        spot_clip_probability(raw_probability, models$calibration$clip_epsilon)
      )
  )

  tibble::tibble(
    raw_probability = raw_probability,
    probability = calibrated_probability,
    classification = spot_zone(calibrated_probability)
  )
}

# Stage 2 strategy lookup —-

spot_stage2_definition <- function(models, strategy) {
  if (!strategy %in% names(models$stage2$strategies)) {
    stop("Unknown Stage 2 strategy: ", strategy, call. = FALSE)
  }
  models$stage2$strategies[[strategy]]
}

# Stage 2 prediction —-

spot_predict_stage2 <- function(stage1_probability, strategy, measures, models = spot_load_models()) {
  probability <- as.numeric(stage1_probability)
  if (length(probability) != 1L || !is.finite(probability) || probability < 0 || probability > 1) {
    stop("Stage 1 probability must be one finite value from 0 to 1.", call. = FALSE)
  }
  if (!identical(spot_zone(probability), "AMBER")) {
    stop("Stage 2 is available only after an Amber Stage 1 result.", call. = FALSE)
  }

  definition <- spot_stage2_definition(models, strategy)
  values <- list()
  imputed <- character()
  for (name in definition$predictors) {
    value <- as.numeric(measures[[name]])
    if (length(value) == 0L || is.na(value)) {
      value <- as.numeric(models$stage2$medians[[name]])
      imputed <- c(imputed, name)
    }
    if (length(value) != 1L || !is.finite(value)) {
      stop("Stage 2 measure must be finite or missing: ", name, call. = FALSE)
    }
    values[[name]] <- value
  }

  linear_predictor <- stats::qlogis(spot_clip_probability(probability)) +
    as.numeric(definition$intercept)
  for (name in definition$predictors) {
    linear_predictor <- linear_predictor +
      as.numeric(definition$coefficients[[name]]) * values[[name]]
  }
  stage2_probability <- stats::plogis(linear_predictor)

  list(
    strategy = strategy,
    label = definition$label,
    probability = stage2_probability,
    classification = unname(spot_zone(stage2_probability)),
    imputed = unname(imputed),
    measures = values
  )
}

# Compatible Stage 2 strategy scoring —-

spot_predict_all_stage2 <- function(stage1_probability, measures, models = spot_load_models()) {
  stats::setNames(
    lapply(spot_stage2_ids, function(id) {
      spot_predict_stage2(stage1_probability, id, measures, models)
    }),
    spot_stage2_ids
  )
}

# Weight-for-age calculation used by the application reference —-

spot_weight_for_age_zscore <- function(age_months, sex, weight, unit) {
  age_months <- suppressWarnings(as.numeric(age_months))
  sex <- suppressWarnings(as.numeric(sex))
  weight <- suppressWarnings(as.numeric(weight))
  unit <- as.character(unit)

  if (length(age_months) != 1L || !is.finite(age_months) || age_months < 1 || age_months > 59) {
    stop("Age must be from 1 to 59 months.", call. = FALSE)
  }
  if (length(sex) != 1L || !is.finite(sex) || !sex %in% c(0, 1)) {
    stop("Sex must use the fixed 0 or 1 coding.", call. = FALSE)
  }
  if (length(weight) != 1L || !is.finite(weight)) {
    stop("Weight must be one finite value.", call. = FALSE)
  }
  if (length(unit) != 1L || !unit %in% c("kg", "lb")) {
    stop("Weight unit must be kg or lb.", call. = FALSE)
  }

  weight_kg <- if (identical(unit, "lb")) weight / 2.2046226218487757 else weight
  if (weight_kg < 0.5 || weight_kg > 50) {
    stop("Weight must be from 0.5 to 50 kg.", call. = FALSE)
  }

  package_sex <- if (identical(sex, 1)) 1 else 2
  age_days <- age_months * (365.25 / 12)
  wfaz <- zscorer::getWGSR(
    sex = package_sex,
    firstPart = weight_kg,
    secondPart = age_days,
    index = "wfa",
    standing = 3
  )
  if (length(wfaz) != 1L || !is.finite(wfaz)) {
    stop("A weight-for-age z score could not be calculated.", call. = FALSE)
  }

  list(wfaz = round(as.numeric(wfaz), 2), weight_kg = weight_kg)
}
