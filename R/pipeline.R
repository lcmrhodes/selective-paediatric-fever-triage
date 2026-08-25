# Reproduction report assembly —-

spot_pipeline_report <- function(state) {
  stage1_validation <- state$frames$s1_baseline
  zones <- table(factor(
    stage1_validation$s1_disposition,
    levels = c("GREEN", "AMBER", "RED")
  ))
  reviewed_cells <- (ncol(state$table_result$reviewed_primary) - 1L) *
    nrow(state$table_result$reviewed_primary)
  list(
    status = "PASS",
    generated_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    cohort_fingerprint = as.list(state$data$cohort_fingerprint),
    stage1 = list(
      fitted_raw_probability_max_abs_difference = state$stage1$raw_max_difference,
      calibration_coefficient_max_abs_difference = state$stage1$calibration_max_difference,
      cambodia_zone_counts = as.list(stats::setNames(as.integer(zones), names(zones)))
    ),
    stage2 = list(
      strategy_count = length(state$stage2$fits),
      severe_stage1_amber_derivation_rows = state$stage2$severe_amber_rows,
      coefficient_max_abs_difference = state$stage2$coefficient_max_difference,
      validation_probability_max_abs_difference = state$stage2$prediction_max_difference
    ),
    bootstrap = as.list(state$bootstrap_check),
    tables = list(
      exact_files = sum(state$table_check),
      expected_files = length(state$table_check),
      reviewed_primary_displayed_cells = reviewed_cells,
      reviewed_primary_expected_cells = 35L
    ),
    figures = as.list(state$figure_check)
  )
}

# Reproduction report output —-

spot_write_pipeline_report <- function(state) {
  report <- spot_pipeline_report(state)
  path <- file.path(
    state$config$paths$outputs,
    "verification",
    "reproduction_report.json"
  )
  jsonlite::write_json(report, path, auto_unbox = TRUE, pretty = TRUE, digits = 17)
  report
}
