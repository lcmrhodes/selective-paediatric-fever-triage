#!/usr/bin/env Rscript

# Browser-parity reference generation —-

suppressPackageStartupMessages({
  library(jsonlite)
  library(Matrix)
  library(recipes)
  library(xgboost)
  library(zscorer)
})

root <- normalizePath(getwd(), mustWork = TRUE)
output_path <- file.path(root, "tests", "fixtures", "browser_parity_reference.json")
source(file.path(root, "R", "model_spec.R"), local = TRUE)
source(file.path(root, "R", "prediction.R"), local = TRUE)
models <- spot_load_models(root)

# Synthetic Stage 1 fixtures —-

fixture_source <- jsonlite::read_json(
  file.path(root, "tests", "fixtures", "prediction_fixtures.json"),
  simplifyVector = FALSE
)

to_input_frame <- function(input) {
  as.data.frame(
    lapply(input, function(value) if (is.null(value)) NA_real_ else as.numeric(value)),
    check.names = FALSE
  )
}

fixed_cases <- lapply(fixture_source$stage1, function(item) {
  list(id = paste0("fixed_", item$id), input = item$input)
})

set.seed(6709)
ranges <- list(
  age.months = c(0, 216),
  wfaz = c(-10, 10),
  cidysymp = c(0, 60),
  hr.all = c(20, 300),
  rr.all = c(5, 150),
  envhtemp = c(25, 45)
)
random_cases <- lapply(seq_len(96), function(index) {
  input <- list(
    age.months = runif(1, ranges$age.months[[1]], ranges$age.months[[2]]),
    sex = sample(c(0, 1), 1),
    adm.recent = sample(c(0, 1), 1),
    wfaz = runif(1, ranges$wfaz[[1]], ranges$wfaz[[2]]),
    cidysymp = runif(1, ranges$cidysymp[[1]], ranges$cidysymp[[2]]),
    not.alert = sample(c(0, 1), 1),
    hr.all = runif(1, ranges$hr.all[[1]], ranges$hr.all[[2]]),
    rr.all = runif(1, ranges$rr.all[[1]], ranges$rr.all[[2]]),
    envhtemp = runif(1, ranges$envhtemp[[1]], ranges$envhtemp[[2]]),
    crt.long = sample(c(0, 1), 1)
  )

  # Each imputation path is exercised without using any study row.
  missing_count <- index %% 7L
  if (missing_count) {
    missing_names <- sample(spot_stage1_predictors, missing_count)
    for (name in missing_names) input[[name]] <- NA_real_
  }
  list(id = sprintf("synthetic_%03d", index), input = input)
})

stage1_cases <- lapply(c(fixed_cases, random_cases), function(item) {
  frame <- to_input_frame(item$input)
  baked <- recipes::bake(models$preprocessor, new_data = frame)
  result <- spot_predict_stage1(frame, models)
  list(
    id = item$id,
    input = item$input,
    features = unname(as.list(as.numeric(baked[1, spot_stage1_features, drop = TRUE]))),
    raw_probability = result$raw_probability[[1]],
    probability = result$probability[[1]],
    classification = result$classification[[1]]
  )
})

# Synthetic Stage 2 fixtures —-

stage2_measure_sets <- list(
  typical = list(spo2 = 98, strem1 = 227, crp = 13.236887, glucose = 5.3888889),
  minimum = list(spo2 = 50, strem1 = 0, crp = 0, glucose = 0),
  maximum = list(spo2 = 100, strem1 = 10000, crp = 1000, glucose = 50),
  missing = list(spo2 = NA_real_, strem1 = NA_real_, crp = NA_real_, glucose = NA_real_)
)
stage2_probabilities <- c(0.005, 0.009924627676747406, 0.02)
stage2_cases <- list()
stage2_index <- 1L
for (probability in stage2_probabilities) {
  for (set_name in names(stage2_measure_sets)) {
    measures <- stage2_measure_sets[[set_name]]
    for (strategy in spot_stage2_ids) {
      result <- spot_predict_stage2(probability, strategy, measures, models)
      stage2_cases[[stage2_index]] <- list(
        id = sprintf("p_%s_%s_%s", format(probability, scientific = FALSE), set_name, strategy),
        stage1_probability = probability,
        strategy = strategy,
        measures = measures,
        probability = result$probability,
        classification = result$classification,
        imputed = unname(as.list(result$imputed))
      )
      stage2_index <- stage2_index + 1L
    }
  }
}

# Weight-for-age fixtures —-

wfaz_inputs <- list(
  list(id = "female_1m_kg", "age.months" = 1, sex = 0, weight = 4.2, unit = "kg"),
  list(id = "male_6m_kg", "age.months" = 6, sex = 1, weight = 7.8, unit = "kg"),
  list(id = "female_24m_kg", "age.months" = 24, sex = 0, weight = 12.4, unit = "kg"),
  list(id = "female_24m_lb", "age.months" = 24, sex = 0, weight = 12.4 * 2.2046226218487757, unit = "lb"),
  list(id = "male_37_5m_kg", "age.months" = 37.5, sex = 1, weight = 15.1, unit = "kg"),
  list(id = "female_59m_kg", "age.months" = 59, sex = 0, weight = 18.5, unit = "kg")
)
wfaz_cases <- lapply(wfaz_inputs, function(input) {
  result <- spot_weight_for_age_zscore(
    input[["age.months"]],
    input$sex,
    input$weight,
    input$unit
  )
  list(
    id = input$id,
    input = input[setdiff(names(input), "id")],
    wfaz = result$wfaz,
    weight_kg = result$weight_kg
  )
})

# Reference file —-

reference <- list(
  reference_implementation = "R",
  fixture_origin = "fixed manual and deterministic synthetic inputs only",
  participant_rows_included = FALSE,
  tolerances = list(
    stage1_features = 1e-12,
    probability = 1e-12,
    weight_for_age = 1e-12
  ),
  stage1 = stage1_cases,
  stage2 = stage2_cases,
  weight_for_age = wfaz_cases
)

jsonlite::write_json(
  reference,
  output_path,
  auto_unbox = TRUE,
  pretty = TRUE,
  digits = 17,
  null = "null",
  na = "null"
)
message("Wrote browser parity reference to ", normalizePath(output_path))
