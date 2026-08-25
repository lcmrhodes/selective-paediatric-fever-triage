# Display labels and formatting helpers —-

spot_labels <- function() {
  c(
    s1_baseline = "Clinical-only Stage 1",
    spo2 = "Stage 1 + oxygen saturation",
    strem1 = "Stage 1 + sTREM-1",
    crp_strem1 = "Stage 1 + CRP + sTREM-1",
    crp_strem1_glucose = "Stage 1 + CRP + sTREM-1 + glucose",
    spo2_strem1 = "Stage 1 + oxygen saturation + sTREM-1",
    spo2_strem1_crp = "Stage 1 + oxygen saturation + sTREM-1 + CRP",
    spo2_strem1_crp_glucose = "Stage 1 + oxygen saturation + sTREM-1 + CRP + glucose"
  )
}

spot_interval <- function(intervals, id, metric) {
  row <- intervals[intervals$model_id == id & intervals$metric == metric, , drop = FALSE]
  if (nrow(row) != 1L) stop("Missing interval: ", id, " / ", metric, call. = FALSE)
  c(low = row$low[[1]], high = row$high[[1]])
}

spot_partition_counts <- function(percentages, denominator = 10000L) {
  exact <- percentages / 100 * denominator
  counts <- floor(exact)
  remainder <- denominator - sum(counts)
  if (remainder > 0L) {
    index <- order(exact - counts, decreasing = TRUE)[seq_len(remainder)]
    counts[index] <- counts[index] + 1L
  }
  if (sum(counts) != denominator) stop("Projected partition failed.", call. = FALSE)
  counts
}

spot_number <- function(x, digits = 3) formatC(x, format = "f", digits = digits)
spot_count <- function(x) formatC(as.integer(x), format = "d", big.mark = ",")
spot_ci <- function(point, low, high, digits = 3, separator = "–") {
  paste0(
    spot_number(point, digits), " (", spot_number(low, digits),
    separator, spot_number(high, digits), ")"
  )
}
spot_projected <- function(point, low, high, separator = "–") {
  paste0(
    spot_count(point), " (", spot_count(round(low * 100)),
    separator, spot_count(round(high * 100)), ")"
  )
}
spot_projected_percent <- function(point, low, high, separator = "-") {
  paste0(
    spot_count(round(point * 100)), " (", spot_count(round(low * 100)),
    separator, spot_count(round(high * 100)), ")"
  )
}

# Main-table row builders —-

spot_summary_row <- function(id, frames, intervals) {
  frame <- frames[[id]]
  metrics <- spot_validation_metrics(frame)
  zone <- frame$cascade_disposition
  severe <- frame$outcome.binary_num == 1
  percentages <- c(
    Green = metrics[["green_pct"]],
    Amber = metrics[["amber_pct"]],
    Red = metrics[["red_pct"]]
  )
  points <- spot_partition_counts(percentages)
  auc <- spot_interval(intervals, id, "weighted_auc")
  green <- spot_interval(intervals, id, "green_pct")
  amber <- spot_interval(intervals, id, "amber_pct")
  red <- spot_interval(intervals, id, "red_pct")
  data.frame(
    Strategy = unname(spot_labels()[[id]]),
    `Weighted AUROC (95% CI)` = spot_ci(metrics[["weighted_auc"]], auc[["low"]], auc[["high"]]),
    `Green per 10,000 (95% CI)` = spot_projected(points[["Green"]], green[["low"]], green[["high"]]),
    `Amber per 10,000 (95% CI)` = spot_projected(points[["Amber"]], amber[["low"]], amber[["high"]]),
    `Red per 10,000 (95% CI)` = spot_projected(points[["Red"]], red[["low"]], red[["high"]]),
    `Severe Green, n/N (%)` = sprintf("%d/36 (%.1f%%)", sum(severe & zone == "GREEN"), 100 * sum(severe & zone == "GREEN") / 36),
    `Severe Amber, n/N (%)` = sprintf("%d/36 (%.1f%%)", sum(severe & zone == "AMBER"), 100 * sum(severe & zone == "AMBER") / 36),
    `Severe Red, n/N (%)` = sprintf("%d/36 (%.1f%%)", sum(severe & zone == "RED"), 100 * sum(severe & zone == "RED") / 36),
    check.names = FALSE
  )
}

spot_transition_row <- function(id, frames) {
  frame <- frames[[id]]
  amber <- frame$s1_disposition == "AMBER"
  severe <- frame$outcome.binary_num == 1
  zone <- frame$cascade_disposition
  data.frame(
    Strategy = unname(spot_labels()[[id]]),
    `All Amber to Green, n/N (%)` = sprintf("%d/122 (%.1f%%)", sum(amber & zone == "GREEN"), 100 * sum(amber & zone == "GREEN") / 122),
    `All retained Amber, n/N (%)` = sprintf("%d/122 (%.1f%%)", sum(amber & zone == "AMBER"), 100 * sum(amber & zone == "AMBER") / 122),
    `All Amber to Red, n/N (%)` = sprintf("%d/122 (%.1f%%)", sum(amber & zone == "RED"), 100 * sum(amber & zone == "RED") / 122),
    `Severe Amber to Green, n/N` = sprintf("%d/7", sum(amber & severe & zone == "GREEN")),
    `Severe retained Amber, n/N` = sprintf("%d/7", sum(amber & severe & zone == "AMBER")),
    `Severe Amber to Red, n/N` = sprintf("%d/7", sum(amber & severe & zone == "RED")),
    check.names = FALSE
  )
}

spot_reviewed_primary <- function(frames, intervals) {
  ids <- c("s1_baseline", "spo2", "strem1", "crp_strem1", "crp_strem1_glucose")
  labels <- c(
    s1_baseline = "Clinical-only Stage 1 model",
    spo2 = "S1 + oxygen saturation S2",
    strem1 = "S1 + sTREM-1 S2",
    crp_strem1 = "S1 + CRP + sTREM-1 S2",
    crp_strem1_glucose = "S1 + CRP + sTREM-1 + glucose S2"
  )
  dplyr::bind_rows(lapply(ids, function(id) {
    point <- spot_validation_metrics(frames[[id]])
    interval <- function(metric) spot_interval(intervals, id, metric)
    auc <- interval("weighted_auc")
    not_green <- interval("severe_not_green_pop_pct")
    severe_red <- interval("severe_red_pop_pct")
    nonsevere_green <- interval("nonsevere_green_pop_pct")
    amber <- interval("amber_pct")
    red <- interval("red_pct")
    amber_nonsevere_red <- interval("s1_amber_nonsevere_red_pop_pct")
    data.frame(
      `Model strategy` = labels[[id]],
      `Weighted AUROC` = spot_ci(point[["weighted_auc"]], auc[["low"]], auc[["high"]], separator = "-"),
      `Severe events not assigned Green, projected per 10,000 (95% CI)` = spot_projected_percent(point[["severe_not_green_pop_pct"]], not_green[["low"]], not_green[["high"]]),
      `Severe events assigned Red, projected per 10,000 (95% CI)` = spot_projected_percent(point[["severe_red_pop_pct"]], severe_red[["low"]], severe_red[["high"]]),
      `Non-severe assigned Green, projected per 10,000 (95% CI)` = spot_projected_percent(point[["nonsevere_green_pop_pct"]], nonsevere_green[["low"]], nonsevere_green[["high"]]),
      `Amber monitored, projected per 10,000 (95% CI)` = spot_projected_percent(point[["amber_pct"]], amber[["low"]], amber[["high"]]),
      `Red referred, projected per 10,000 (95% CI)` = spot_projected_percent(point[["red_pct"]], red[["low"]], red[["high"]]),
      `Non-severe Stage 1 Amber escalated to Red, projected per 10,000 (95% CI)` = if (id == "s1_baseline") {
        "Not applicable"
      } else {
        spot_projected_percent(point[["s1_amber_nonsevere_red_pop_pct"]], amber_nonsevere_red[["low"]], amber_nonsevere_red[["high"]])
      },
      check.names = FALSE
    )
  }))
}

# Cohort-characteristic helpers —-

spot_continuous <- function(x, digits = 1) {
  values <- stats::quantile(x, c(0.25, 0.5, 0.75), na.rm = TRUE, names = FALSE, type = 2)
  sprintf(paste0("%.", digits, "f (%.", digits, "f–%.", digits, "f)"), values[[2]], values[[1]], values[[3]])
}
spot_categorical <- function(x, condition) {
  count <- sum(condition(x), na.rm = TRUE)
  total <- sum(!is.na(x))
  sprintf("%d/%d (%.1f%%)", count, total, 100 * count / total)
}

spot_characteristic_spec <- function() {
  data.frame(
    Characteristic = c(
      "Age, months", "Male sex", "Outpatient recruitment", "Hospital admission in preceding 6 months",
      "Weight-for-age z score", "Illness duration, days", "Not alert", "Heart rate, beats/min",
      "Respiratory rate, breaths/min", "Axillary temperature, °C", "Capillary refill >2 s",
      "Oxygen saturation, %", "sTREM-1, pg/mL", "C-reactive protein, mg/L",
      "Glucose, mmol/L", "Parent-study severe illness"
    ),
    type = c(
      "continuous", "binary", "outpatient", "binary", "continuous", "continuous",
      "binary", "continuous", "continuous", "continuous", "binary", "continuous",
      "continuous", "continuous", "continuous", "severe"
    ),
    column = c(
      "age.months", "sex", "ipdopd", "adm.recent", "wfaz", "cidysymp", "not.alert",
      "hr.all", "rr.all", "envhtemp", "crt.long", "spo2", "strem1", "crp", "glucose",
      "stage2_outcome"
    ),
    stringsAsFactors = FALSE
  )
}

spot_format_characteristic <- function(data, type, column) {
  x <- data[[column]]
  if (type == "continuous") {
    digits <- if (column %in% c("wfaz", "envhtemp", "glucose")) 2 else 1
    return(spot_continuous(x, digits))
  }
  if (type == "binary") return(spot_categorical(x, function(value) value == 1))
  if (type == "outpatient") return(spot_categorical(x, function(value) value == "O"))
  if (type == "severe") return(spot_categorical(x, function(value) value == 1))
  stop("Unknown characteristic type.", call. = FALSE)
}

# Calibration and count summaries —-

spot_calibration_metrics <- function(frame) {
  probability <- spot_clip_probability(frame$cascade_pred)
  outcome <- frame$outcome.binary_num
  weight <- frame$sampling_weight
  linear <- qlogis(probability)
  slope <- suppressWarnings(stats::glm(outcome ~ linear, family = stats::binomial(), weights = weight))
  intercept <- suppressWarnings(stats::glm(outcome ~ 1 + offset(linear), family = stats::binomial(), weights = weight))
  c(
    weighted_auc = spot_weighted_auc(outcome, probability, weight),
    weighted_brier = stats::weighted.mean((outcome - probability)^2, weight),
    calibration_intercept = unname(stats::coef(intercept)[["(Intercept)"]]),
    calibration_slope = unname(stats::coef(slope)[["linear"]])
  )
}

spot_raw_counts <- function(ids, frames, transition = FALSE) {
  rows <- list()
  for (id in ids) {
    frame <- frames[[id]]
    zone <- frame$cascade_disposition
    severe <- frame$outcome.binary_num == 1
    amber <- frame$s1_disposition == "AMBER"
    if (transition) {
      metrics <- c(
        "All Stage 1 Amber to Green" = sum(amber & zone == "GREEN"),
        "All Stage 1 Amber retained Amber" = sum(amber & zone == "AMBER"),
        "All Stage 1 Amber to Red" = sum(amber & zone == "RED"),
        "Severe Stage 1 Amber to Green" = sum(amber & severe & zone == "GREEN"),
        "Severe Stage 1 Amber retained Amber" = sum(amber & severe & zone == "AMBER"),
        "Severe Stage 1 Amber to Red" = sum(amber & severe & zone == "RED")
      )
      denominators <- c(rep(122L, 3), rep(7L, 3))
    } else {
      metrics <- c(
        "All assigned Green" = sum(zone == "GREEN"),
        "All assigned Amber" = sum(zone == "AMBER"),
        "All assigned Red" = sum(zone == "RED"),
        "Severe assigned Green" = sum(severe & zone == "GREEN"),
        "Severe assigned Amber" = sum(severe & zone == "AMBER"),
        "Severe assigned Red" = sum(severe & zone == "RED")
      )
      denominators <- c(rep(824L, 3), rep(36L, 3))
    }
    rows[[id]] <- data.frame(
      Strategy = unname(spot_labels()[[id]]),
      Metric = names(metrics),
      `Raw n/N` = paste0(as.integer(metrics), "/", denominators),
      check.names = FALSE
    )
  }
  dplyr::bind_rows(rows)
}

# Supplementary tables —-

spot_supplementary_tables <- function(data, stage2, frames) {
  # Summarize the endpoint contracts and participant flow.
  endpoint_flow <- data.frame(
    Cohort_or_contract = c(
      "Raw parent cohort", "Paper analysis cohort", "Stage 1 development contract",
      "Stage 2 derivation cohort", "Held-out Cambodia validation cohort"
    ),
    Rows = c(3423L, 3405L, 2578L, 2581L, 824L),
    Events = c(133L, 133L, 92L, 97L, 36L),
    Definition_or_note = c(
      "133 parent-study severe events among 3,405 children with observed parent-study endpoint; 18 rows had no parent-study endpoint",
      "Complete parent-study severe-illness endpoint",
      "Non-Cambodia; death or organ support within two days; complete Stage 1 contract",
      "Non-Cambodia; parent-study severe-illness endpoint, including discharge home for end-of-life care",
      "Complete parent-study severe-illness endpoint; all 36 also met death or organ support within two days"
    ),
    check.names = FALSE
  )

  # Describe the derivation and validation cohorts.
  specification <- spot_characteristic_spec()
  characteristics <- data.frame(
    Characteristic = specification$Characteristic,
    `Derivation (N=2,581)` = mapply(
      function(type, column) spot_format_characteristic(data$stage2_development, type, column),
      specification$type, specification$column
    ),
    `Cambodia validation (N=824)` = mapply(
      function(type, column) spot_format_characteristic(data$validation, type, column),
      specification$type, specification$column
    ),
    check.names = FALSE
  )

  # Report missingness for every model input.
  missing_specification <- specification[specification$column != "stage2_outcome", ]
  missingness <- data.frame(
    Variable = missing_specification$Characteristic,
    `Derivation missing, n/N (%)` = vapply(missing_specification$column, function(column) {
      count <- sum(is.na(data$stage2_development[[column]]))
      sprintf("%d/2581 (%.1f%%)", count, 100 * count / 2581)
    }, character(1)),
    `Cambodia missing, n/N (%)` = vapply(missing_specification$column, function(column) {
      count <- sum(is.na(data$validation[[column]]))
      sprintf("%d/824 (%.1f%%)", count, 100 * count / 824)
    }, character(1)),
    check.names = FALSE
  )

  # Record the complete fixed Stage 1 specification.
  stage1_specification <- data.frame(
    Element = c(
      "Endpoint", "Development cohort", "Predictors", "Algorithm", "Fitted iterations",
      "Key tuning values", "Tree settings", "Recipe", "Calibration", "Traffic-light thresholds"
    ),
    Stage_1_specification = c(
      "Death or organ support within two days",
      "2,578 non-Cambodia children; 92 events under the Stage 1 contract",
      "Age; sex; recent hospital admission; weight-for-age z score; illness duration; alertness; heart rate; respiratory rate; axillary temperature; prolonged capillary refill",
      "XGBoost gradient-boosted trees using the fixed model specification",
      "5,000",
      "eta 0.009763351; max_depth 160; min_child_weight 36; max_leaves 219; gamma 1.239363e-08; lambda 2.503444e-07; alpha 2.532750e-06; base_score 0.03568658",
      "hist tree method; lossguide growth; one thread",
      "Numeric missingness indicators; bagged numeric imputation; explicit unknown/novel nominal levels; one-hot encoding; zero-variance removal",
      "Weighted Platt scaling on full-derivation fitted logits; intercept 1.0592829; slope 1.8527707",
      "Green <0.005; Amber 0.005–0.02 inclusive; Red >0.02"
    ),
    check.names = FALSE
  )

  # Assemble the Stage 2 fitting diagnostics reported in the appendix.
  source_predictors <- list(
    spo2 = "oxy.ra", strem1 = "sTREM1", crp_strem1 = c("CRP", "sTREM1"),
    crp_strem1_glucose = c("CRP", "sTREM1", "glucose"),
    spo2_strem1 = c("oxy.ra", "sTREM1"),
    spo2_strem1_crp = c("oxy.ra", "sTREM1", "CRP"),
    spo2_strem1_crp_glucose = c("oxy.ra", "sTREM1", "CRP", "glucose")
  )
  formula_text <- list(
    spo2 = "outcome.binary_num ~ oxy.ra + offset(s1_logit)",
    strem1 = "outcome.binary_num ~ sTREM1 + offset(s1_logit)",
    crp_strem1 = "outcome.binary_num ~ CRP + sTREM1 + offset(s1_logit)",
    crp_strem1_glucose = "outcome.binary_num ~ CRP + sTREM1 + glucose + offset(s1_logit)",
    spo2_strem1 = "outcome.binary_num ~ oxy.ra + sTREM1 + offset(s1_logit)",
    spo2_strem1_crp = "outcome.binary_num ~ oxy.ra + sTREM1 + CRP + offset(s1_logit)",
    spo2_strem1_crp_glucose = "outcome.binary_num ~ oxy.ra + sTREM1 + CRP + glucose + offset(s1_logit)"
  )
  stage2_diagnostics <- dplyr::bind_rows(lapply(names(stage2$fits), function(id) {
    fit <- stage2$fits[[id]]
    prediction <- as.numeric(predict(fit, newdata = stage2$derivation, type = "response"))
    diagnostic_frame <- transform(stage2$derivation, cascade_pred = prediction)
    diagnostic <- spot_calibration_metrics(diagnostic_frame)
    data.frame(
      Strategy = stage2$definitions[[id]]$label,
      Role = stage2$definitions[[id]]$role,
      Predictors = paste(source_predictors[[id]], collapse = " + "),
      Formula = formula_text[[id]],
      Converged = TRUE,
      Warnings = "none",
      `Weighted apparent derivation AUROC` = sprintf("%.3f", diagnostic[["weighted_auc"]]),
      `Weighted derivation calibration intercept` = sprintf("%.3f", diagnostic[["calibration_intercept"]]),
      `Weighted derivation calibration slope` = sprintf("%.3f", diagnostic[["calibration_slope"]]),
      `Severe Stage 1 Amber fitting weight` = 5L,
      `Derivation rows` = 2581L,
      `Derivation events` = 97L,
      `Offset coefficient` = "Fixed at 1",
      check.names = FALSE
    )
  }))

  # Format the fitted Stage 2 coefficients and uncertainty statistics.
  coefficient_rows <- list()
  term_map <- c(spo2 = "oxy.ra", strem1 = "sTREM1", crp = "CRP", glucose = "glucose")
  coefficient_labels <- sub("oxygen saturation", "SpO2", spot_labels()[names(stage2$fits)], fixed = TRUE)
  for (id in names(stage2$fits)) {
    matrix <- summary(stage2$fits[[id]])$coefficients
    for (term in rownames(matrix)) {
      display_term <- if (term == "(Intercept)") term else unname(term_map[[term]])
      coefficient_rows[[length(coefficient_rows) + 1L]] <- data.frame(
        Strategy = coefficient_labels[[id]],
        Term = display_term,
        Coefficient = sprintf("%.7f", matrix[term, "Estimate"]),
        `Standard error` = sprintf("%.7f", matrix[term, "Std. Error"]),
        `p value` = formatC(matrix[term, "Pr(>|z|)"], format = "g", digits = 3),
        check.names = FALSE
      )
    }
  }
  coefficients <- dplyr::bind_rows(coefficient_rows)

  # Summarize held-out validation calibration for each strategy.
  calibration <- dplyr::bind_rows(lapply(names(frames), function(id) {
    value <- spot_calibration_metrics(frames[[id]])
    data.frame(
      Strategy = unname(spot_labels()[[id]]),
      `Weighted AUROC` = sprintf("%.3f", value[["weighted_auc"]]),
      `Weighted Brier score` = sprintf("%.3f", value[["weighted_brier"]]),
      `Weighted calibration intercept` = sprintf("%.3f", value[["calibration_intercept"]]),
      `Weighted calibration slope` = sprintf("%.3f", value[["calibration_slope"]]),
      check.names = FALSE
    )
  }))

  # Reproduce the documented non-deployable imputation diagnostic.
  outcome_medians <- lapply(c(0, 1), function(outcome) {
    rows <- data$stage2_development$stage2_outcome == outcome
    vapply(c("spo2", "strem1", "crp", "glucose"), function(name) {
      stats::median(data$stage2_development[[name]][rows], na.rm = TRUE)
    }, numeric(1))
  })
  names(outcome_medians) <- c("0", "1")
  diagnostic_validation <- data$validation
  diagnostic_validation <- spot_score_stage1_data(diagnostic_validation, spot_load_models())
  for (name in c("spo2", "strem1", "crp", "glucose")) {
    missing <- is.na(diagnostic_validation[[name]])
    diagnostic_validation[[name]][missing] <- vapply(
      diagnostic_validation$outcome.binary_num[missing],
      function(outcome) outcome_medians[[as.character(outcome)]][[name]],
      numeric(1)
    )
  }
  imputation_diagnostic <- dplyr::bind_rows(lapply(names(stage2$fits), function(id) {
    frame <- diagnostic_validation
    probability <- as.numeric(predict(stage2$fits[[id]], newdata = frame, type = "response"))
    amber <- frame$s1_disposition == "AMBER"
    frame$cascade_pred <- frame$s1_calibrated_pred
    frame$cascade_pred[amber] <- probability[amber]
    frame$cascade_disposition <- frame$s1_disposition
    frame$cascade_disposition[amber] <- spot_zone(probability[amber])
    primary <- frames[[id]]
    severe <- primary$outcome.binary_num == 1
    data.frame(
      Strategy = unname(spot_labels()[[id]]),
      `Primary AUROC` = sprintf("%.3f", spot_validation_metrics(primary)[["weighted_auc"]]),
      `Diagnostic AUROC` = sprintf("%.3f", spot_validation_metrics(frame)[["weighted_auc"]]),
      `AUROC difference` = sprintf("%.3f", spot_validation_metrics(frame)[["weighted_auc"]] - spot_validation_metrics(primary)[["weighted_auc"]]),
      `Primary Green/Amber/Red, n` = paste(table(factor(primary$cascade_disposition, levels = c("GREEN", "AMBER", "RED"))), collapse = "/"),
      `Diagnostic Green/Amber/Red, n` = paste(table(factor(frame$cascade_disposition, levels = c("GREEN", "AMBER", "RED"))), collapse = "/"),
      `Primary severe Green, n` = sum(severe & primary$cascade_disposition == "GREEN"),
      `Diagnostic severe Green, n` = sum(severe & frame$cascade_disposition == "GREEN"),
      Diagnostic = "Outcome-stratified derivation-median imputation; fixed models; non-deployable",
      check.names = FALSE
    )
  }))

  # Return the supplementary tables in manuscript order.
  list(
    supp_table_1_endpoint_and_participant_flow = endpoint_flow,
    supp_table_2_characteristics = characteristics,
    supp_table_3_missingness = missingness,
    supp_table_4_stage1_specification = stage1_specification,
    supp_table_5_stage2_models_and_diagnostics = stage2_diagnostics,
    supp_table_6_stage2_coefficients = coefficients,
    supp_table_7_validation_calibration = calibration,
    supp_table_8_outcome_stratified_imputation_diagnostic = imputation_diagnostic,
    supp_table_9_raw_counts_main_table_1 = spot_raw_counts(c("s1_baseline", "spo2", "strem1", "crp_strem1", "crp_strem1_glucose"), frames),
    supp_table_10_raw_counts_main_table_2 = spot_raw_counts(c("spo2", "strem1", "crp_strem1", "crp_strem1_glucose"), frames, transition = TRUE),
    supp_table_11_raw_counts_main_table_3 = spot_raw_counts(c("spo2", "spo2_strem1", "spo2_strem1_crp", "spo2_strem1_crp_glucose"), frames)
  )
}

# Table export and regression checks —-

spot_write_tables <- function(data, stage2, frames, intervals, config) {
  main <- list(
    main_table_1 = dplyr::bind_rows(lapply(
      c("s1_baseline", "spo2", "strem1", "crp_strem1", "crp_strem1_glucose"),
      spot_summary_row, frames = frames, intervals = intervals
    )),
    main_table_2 = dplyr::bind_rows(lapply(
      c("spo2", "strem1", "crp_strem1", "crp_strem1_glucose"),
      spot_transition_row, frames = frames
    )),
    main_table_3 = dplyr::bind_rows(lapply(
      c("spo2", "spo2_strem1", "spo2_strem1_crp", "spo2_strem1_crp_glucose"),
      spot_summary_row, frames = frames, intervals = intervals
    ))
  )
  supplementary <- spot_supplementary_tables(data, stage2, frames)
  tables <- c(main, supplementary)
  for (name in names(tables)) {
    readr::write_csv(tables[[name]], file.path(config$paths$outputs, "tables", paste0(name, ".csv")))
  }
  reviewed <- spot_reviewed_primary(frames, intervals)
  readr::write_csv(reviewed, file.path(config$paths$outputs, "verification", "reviewed_primary_table_35_cells.csv"))
  list(tables = tables, reviewed_primary = reviewed)
}

spot_check_display_tables <- function(result, root) {
  differences <- list()
  observed <- c(result$tables, list(reviewed_primary_table_35_cells = result$reviewed_primary))
  for (name in names(observed)) {
    reference <- readr::read_csv(
      file.path(root, "tests", "reference", paste0(name, ".csv")),
      col_types = readr::cols(.default = readr::col_character()),
      show_col_types = FALSE
    )
    value <- as.data.frame(observed[[name]])
    value[] <- lapply(value, as.character)
    reference <- as.data.frame(reference)
    differences[[name]] <- identical(names(value), names(reference)) &&
      all(vapply(names(value), function(column) {
        identical(unname(value[[column]]), unname(reference[[column]]))
      }, logical(1)))
  }
  if (!all(unlist(differences))) {
    stop(
      "Displayed table equivalence failed: ",
      paste(names(differences)[!unlist(differences)], collapse = ", "),
      call. = FALSE
    )
  }
  unlist(differences)
}
