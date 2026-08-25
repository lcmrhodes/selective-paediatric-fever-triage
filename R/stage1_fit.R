# Stage 1 analysis frame —-

spot_stage1_frame <- function(data) {
  frame <- data.frame(
    label = data$label,
    site = data$site,
    ipdopd = data$ipdopd,
    sampling_weight = data$sampling_weight,
    data[, spot_stage1_predictors, drop = FALSE],
    outcome.binary = factor(data$stage1_outcome, levels = c(0, 1)),
    check.names = FALSE
  )
  frame
}

# Stage 1 model fitting and verification —-

spot_fit_stage1 <- function(development, models, config) {
  training <- spot_stage1_frame(development)
  set.seed(as.integer(config$analysis$seed))

  # Recreate the published preprocessing recipe.
  recipe <- recipes::recipe(outcome.binary ~ ., data = training) |>
    recipes::update_role(label, site, ipdopd, sampling_weight, new_role = "id") |>
    recipes::step_indicate_na(recipes::all_numeric_predictors()) |>
    recipes::step_impute_bag(
      recipes::all_numeric_predictors(),
      -recipes::has_role("id"),
      -recipes::all_outcomes(),
      trees = spot_stage1_parameters$imputation_trees,
      seed_val = spot_stage1_parameters$imputation_seed,
      options = list(keepX = FALSE)
    ) |>
    recipes::step_unknown(recipes::all_nominal_predictors()) |>
    recipes::step_novel(recipes::all_nominal_predictors()) |>
    recipes::step_dummy(recipes::all_nominal_predictors(), one_hot = TRUE) |>
    recipes::step_zv(recipes::all_predictors())

  # Apply the fixed XGBoost specification.
  specification <- parsnip::boost_tree(
    trees = spot_stage1_parameters$trees,
    tree_depth = spot_stage1_parameters$max_depth,
    learn_rate = spot_stage1_parameters$eta,
    loss_reduction = spot_stage1_parameters$gamma,
    min_n = spot_stage1_parameters$min_child_weight,
    sample_size = 1
  ) |>
    parsnip::set_engine(
      "xgboost",
      objective = "binary:logistic",
      tree_method = spot_stage1_parameters$tree_method,
      grow_policy = spot_stage1_parameters$grow_policy,
      max_leaves = spot_stage1_parameters$max_leaves,
      lambda = spot_stage1_parameters$lambda,
      alpha = spot_stage1_parameters$alpha,
      base_score = spot_stage1_parameters$base_score,
      eval_metric = "aucpr",
      nthread = spot_stage1_parameters$nthread
    ) |>
    parsnip::set_mode("classification")

  # Fit the full Stage 1 workflow on the development cohort.
  workflow <- workflows::workflow() |>
    workflows::add_recipe(recipe) |>
    workflows::add_model(specification) |>
    parsnip::fit(data = training)

  engine <- workflows::extract_fit_engine(workflow)
  if (!identical(as.integer(engine$niter), spot_stage1_parameters$trees)) {
    stop("Stage 1 did not complete the fixed boosting iteration count.", call. = FALSE)
  }

  # Compare rebuilt raw predictions with the distributable scorer.
  fitted_raw <- as.numeric(predict(workflow, training, type = "prob")$.pred_1)
  public <- spot_predict_stage1(training, models)
  raw_max_difference <- max(abs(fitted_raw - public$raw_probability))

  # Refit and verify the weighted Platt calibration.
  calibration_data <- data.frame(
    observed = as.integer(as.character(training$outcome.binary) == "1"),
    linear_predictor = qlogis(spot_clip_probability(fitted_raw)),
    sampling_weight = training$sampling_weight
  )
  calibration_fit <- stats::glm(
    observed ~ linear_predictor,
    data = calibration_data,
    weights = sampling_weight,
    family = stats::binomial()
  )
  coefficients <- c(
    intercept = unname(stats::coef(calibration_fit)[["(Intercept)"]]),
    slope = unname(stats::coef(calibration_fit)[["linear_predictor"]])
  )
  expected <- c(
    intercept = as.numeric(models$calibration$intercept),
    slope = as.numeric(models$calibration$slope)
  )
  calibration_max_difference <- max(abs(coefficients - expected))

  tolerance <- as.numeric(config$analysis$probability_tolerance)
  if (raw_max_difference > tolerance || calibration_max_difference > tolerance) {
    stop(
      "Stage 1 fitted-model equivalence failed: raw=", format(raw_max_difference, digits = 17),
      ", calibration=", format(calibration_max_difference, digits = 17),
      call. = FALSE
    )
  }

  saveRDS(workflow, file.path(config$paths$derived, "stage1_fitted_workflow.rds"))
  saveRDS(calibration_fit, file.path(config$paths$derived, "stage1_calibration_fit.rds"))

  list(
    workflow = workflow,
    calibration_fit = calibration_fit,
    coefficients = coefficients,
    raw_max_difference = raw_max_difference,
    calibration_max_difference = calibration_max_difference
  )
}
