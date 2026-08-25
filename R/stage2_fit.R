# Stage 1 scores for Stage 2 cohorts —-

spot_score_stage1_data <- function(data, models) {
  scored <- spot_predict_stage1(data, models)
  data$s1_raw_pred <- scored$raw_probability
  data$s1_calibrated_pred <- scored$probability
  data$s1_logit <- qlogis(spot_clip_probability(scored$probability))
  data$s1_disposition <- scored$classification
  data$outcome.binary_num <- as.integer(data$stage2_outcome == 1)
  data
}

# Derivation-only biomarker imputation —-

spot_stage2_medians <- function(data) {
  vapply(c("spo2", "strem1", "crp", "glucose"), function(name) {
    stats::median(data[[name]], na.rm = TRUE)
  }, numeric(1))
}

spot_impute_stage2 <- function(data, medians) {
  out <- data
  for (name in names(medians)) out[[name]][is.na(out[[name]])] <- medians[[name]]
  out
}

# Reported Stage 2 strategy definitions —-

spot_stage2_definitions <- function() {
  list(
    spo2 = list(label = "Stage 1 + oxygen saturation", predictors = "spo2", formula = outcome.binary_num ~ spo2 + offset(s1_logit), role = "Primary"),
    strem1 = list(label = "Stage 1 + sTREM-1", predictors = "strem1", formula = outcome.binary_num ~ strem1 + offset(s1_logit), role = "Primary"),
    crp_strem1 = list(label = "Stage 1 + CRP + sTREM-1", predictors = c("crp", "strem1"), formula = outcome.binary_num ~ crp + strem1 + offset(s1_logit), role = "Primary"),
    crp_strem1_glucose = list(label = "Stage 1 + CRP + sTREM-1 + glucose", predictors = c("crp", "strem1", "glucose"), formula = outcome.binary_num ~ crp + strem1 + glucose + offset(s1_logit), role = "Primary"),
    spo2_strem1 = list(label = "Stage 1 + oxygen saturation + sTREM-1", predictors = c("spo2", "strem1"), formula = outcome.binary_num ~ spo2 + strem1 + offset(s1_logit), role = "Nested sensitivity"),
    spo2_strem1_crp = list(label = "Stage 1 + oxygen saturation + sTREM-1 + CRP", predictors = c("spo2", "strem1", "crp"), formula = outcome.binary_num ~ spo2 + strem1 + crp + offset(s1_logit), role = "Nested sensitivity"),
    spo2_strem1_crp_glucose = list(label = "Stage 1 + oxygen saturation + sTREM-1 + CRP + glucose", predictors = c("spo2", "strem1", "crp", "glucose"), formula = outcome.binary_num ~ spo2 + strem1 + crp + glucose + offset(s1_logit), role = "Nested sensitivity")
  )
}

# Stage 2 fitting and equivalence checks —-

spot_fit_stage2 <- function(development, validation, models, config) {
  derivation_scored <- spot_score_stage1_data(development, models)
  validation_scored <- spot_score_stage1_data(validation, models)
  medians <- spot_stage2_medians(derivation_scored)
  expected_medians <- unlist(models$stage2$medians, use.names = TRUE)
  if (max(abs(medians[names(expected_medians)] - expected_medians)) >
      as.numeric(config$analysis$probability_tolerance)) {
    stop("Stage 2 derivation-median equivalence failed.", call. = FALSE)
  }

  # Apply medians calculated from the derivation cohort only.
  derivation_model <- spot_impute_stage2(derivation_scored, medians)
  validation_model <- spot_impute_stage2(validation_scored, medians)
  definitions <- spot_stage2_definitions()
  # Apply the fixed severe-Amber fitting weight.
  fitting_weight <- derivation_model$sampling_weight
  severe_amber <- derivation_model$outcome.binary_num == 1 &
    derivation_model$s1_disposition == "AMBER"
  fitting_weight[severe_amber] <-
    fitting_weight[severe_amber] * spot_stage2_severe_amber_weight()
  derivation_model$stage2_weight <- fitting_weight

  # Refit each reported strategy and compare coefficients and validation predictions.
  fits <- list()
  maximum_coefficient_difference <- 0
  maximum_prediction_difference <- 0
  for (id in names(definitions)) {
    definition <- definitions[[id]]
    fit <- stats::glm(
      definition$formula,
      data = derivation_model,
      family = stats::binomial(),
      weights = stage2_weight
    )
    if (!isTRUE(fit$converged)) stop("Stage 2 fit did not converge: ", definition$label, call. = FALSE)

    published <- models$stage2$strategies[[id]]
    expected <- c(
      `(Intercept)` = as.numeric(published$intercept),
      unlist(published$coefficients, use.names = TRUE)
    )
    observed <- stats::coef(fit)[names(expected)]
    maximum_coefficient_difference <- max(
      maximum_coefficient_difference,
      abs(observed - expected)
    )

    fitted_prediction <- as.numeric(predict(fit, newdata = validation_model, type = "response"))
    public_prediction <- stats::plogis(
      validation_model$s1_logit + expected[["(Intercept)"]] +
        Reduce(`+`, lapply(definition$predictors, function(name) {
          expected[[name]] * validation_model[[name]]
        }))
    )
    maximum_prediction_difference <- max(
      maximum_prediction_difference,
      abs(fitted_prediction - public_prediction)
    )
    fits[[id]] <- fit
    saveRDS(fit, file.path(config$paths$derived, paste0("stage2_", id, "_fit.rds")))
  }

  tolerance <- as.numeric(config$analysis$probability_tolerance)
  if (maximum_coefficient_difference > tolerance || maximum_prediction_difference > tolerance) {
    stop(
      "Stage 2 fitted-model equivalence failed: coefficients=",
      format(maximum_coefficient_difference, digits = 17),
      ", predictions=", format(maximum_prediction_difference, digits = 17),
      call. = FALSE
    )
  }

  list(
    derivation = derivation_model,
    validation = validation_model,
    definitions = definitions,
    fits = fits,
    medians = medians,
    severe_amber_rows = sum(severe_amber),
    coefficient_max_difference = maximum_coefficient_difference,
    prediction_max_difference = maximum_prediction_difference
  )
}

# Validation frames for reported strategies —-

spot_validation_frames <- function(stage2) {
  base <- stage2$validation
  frames <- list(
    s1_baseline = transform(
      base,
      cascade_pred = s1_calibrated_pred,
      cascade_disposition = s1_disposition
    )
  )
  for (id in names(stage2$fits)) {
    frame <- base
    stage2_probability <- as.numeric(predict(stage2$fits[[id]], newdata = base, type = "response"))
    # Stage 2 can replace probabilities only for Stage 1 Amber rows.
    amber <- frame$s1_disposition == "AMBER"
    frame$cascade_pred <- frame$s1_calibrated_pred
    frame$cascade_pred[amber] <- stage2_probability[amber]
    frame$cascade_disposition <- frame$s1_disposition
    frame$cascade_disposition[amber] <- spot_zone(stage2_probability[amber])
    frames[[id]] <- frame
  }
  frames
}
