# Weighted validation metrics —-

spot_weighted_auc <- function(outcome, prediction, weight) {
  keep <- !is.na(outcome) & !is.na(prediction) & !is.na(weight) &
    is.finite(prediction) & is.finite(weight) & weight > 0
  outcome <- as.integer(outcome[keep])
  prediction <- prediction[keep]
  weight <- weight[keep]
  event_weight <- sum(weight[outcome == 1])
  nonevent_weight <- sum(weight[outcome == 0])
  if (event_weight <= 0 || nonevent_weight <= 0) return(NA_real_)
  order <- order(prediction)
  prediction <- prediction[order]
  outcome <- outcome[order]
  weight <- weight[order]
  runs <- rle(prediction)
  run_id <- rep(seq_along(runs$lengths), runs$lengths)
  event <- rowsum(weight * (outcome == 1), run_id, reorder = FALSE)[, 1]
  nonevent <- rowsum(weight * (outcome == 0), run_id, reorder = FALSE)[, 1]
  before <- c(0, head(cumsum(nonevent), -1))
  sum(event * (before + 0.5 * nonevent)) / (event_weight * nonevent_weight)
}

spot_weighted_percent <- function(condition, denominator, weight) {
  total <- sum(weight[denominator], na.rm = TRUE)
  if (total <= 0) return(NA_real_)
  sum(weight[condition & denominator], na.rm = TRUE) / total * 100
}

spot_validation_metrics <- function(frame) {
  zone <- frame$cascade_disposition
  severe <- frame$outcome.binary_num == 1
  nonsevere <- frame$outcome.binary_num == 0
  amber <- frame$s1_disposition == "AMBER"
  all <- rep(TRUE, nrow(frame))
  weight <- frame$sampling_weight
  c(
    weighted_auc = spot_weighted_auc(frame$outcome.binary_num, frame$cascade_pred, weight),
    severe_not_green_pop_pct = spot_weighted_percent(severe & zone != "GREEN", all, weight),
    severe_red_pop_pct = spot_weighted_percent(severe & zone == "RED", all, weight),
    nonsevere_green_pct = spot_weighted_percent(nonsevere & zone == "GREEN", nonsevere, weight),
    nonsevere_green_pop_pct = spot_weighted_percent(nonsevere & zone == "GREEN", all, weight),
    green_pct = spot_weighted_percent(zone == "GREEN", all, weight),
    amber_pct = spot_weighted_percent(zone == "AMBER", all, weight),
    red_pct = spot_weighted_percent(zone == "RED", all, weight),
    s1_amber_retained_amber_pct = spot_weighted_percent(amber & zone == "AMBER", amber, weight),
    s1_amber_retained_amber_pop_pct = spot_weighted_percent(amber & zone == "AMBER", all, weight),
    s1_amber_green_pop_pct = spot_weighted_percent(amber & zone == "GREEN", all, weight),
    s1_amber_escalated_red_pct = spot_weighted_percent(amber & zone == "RED", amber, weight),
    s1_amber_escalated_red_pop_pct = spot_weighted_percent(amber & zone == "RED", all, weight),
    s1_amber_severe_red_pop_pct = spot_weighted_percent(amber & severe & zone == "RED", all, weight),
    s1_amber_severe_green_pop_pct = spot_weighted_percent(amber & severe & zone == "GREEN", all, weight),
    s1_amber_nonsevere_green_pct = spot_weighted_percent(amber & nonsevere & zone == "GREEN", amber & nonsevere, weight),
    s1_amber_nonsevere_green_pop_pct = spot_weighted_percent(amber & nonsevere & zone == "GREEN", all, weight),
    s1_amber_nonsevere_red_pct = spot_weighted_percent(amber & nonsevere & zone == "RED", amber & nonsevere, weight),
    s1_amber_nonsevere_red_pop_pct = spot_weighted_percent(amber & nonsevere & zone == "RED", all, weight)
  )
}

# Stratified bootstrap resampling —-

spot_bootstrap_indices <- function(frame, replicates, seed) {
  strata <- frame |>
    dplyr::mutate(.row_id = dplyr::row_number()) |>
    dplyr::group_by(outcome.binary_num, s1_disposition) |>
    dplyr::summarise(row_ids = list(.row_id), .groups = "drop")
  set.seed(as.integer(seed))
  lapply(seq_len(as.integer(replicates)), function(index) {
    unlist(lapply(strata$row_ids, function(ids) {
      sample(ids, length(ids), replace = TRUE)
    }), use.names = FALSE)
  })
}

spot_bootstrap_intervals <- function(frames, config) {
  indices <- spot_bootstrap_indices(
    frames$s1_baseline,
    config$analysis$bootstrap_replicates,
    config$analysis$bootstrap_seed
  )
  rows <- list()
  for (id in names(frames)) {
    message("Bootstrap: ", id)
    values <- vapply(indices, function(index) {
      spot_validation_metrics(frames[[id]][index, , drop = FALSE])
    }, numeric(length(spot_validation_metrics(frames[[id]]))))
    for (metric in rownames(values)) {
      interval <- stats::quantile(values[metric, ], c(0.025, 0.975), type = 6, na.rm = TRUE)
      rows[[length(rows) + 1L]] <- data.frame(
        model_id = id,
        metric = metric,
        low = unname(interval[[1]]),
        high = unname(interval[[2]])
      )
    }
  }
  result <- dplyr::bind_rows(rows)
  result[order(result$model_id, result$metric), , drop = FALSE]
}

# Bootstrap regression checks —-

spot_check_bootstrap <- function(intervals, root, tolerance = 5e-12) {
  reference <- read.csv(file.path(root, "tests", "reference", "bootstrap_intervals_5000.csv"))
  observed <- intervals[intervals$metric != "green_pct", names(reference), drop = FALSE]
  observed <- observed[order(observed$model_id, observed$metric), ]
  reference <- reference[order(reference$model_id, reference$metric), ]
  if (!identical(observed$model_id, reference$model_id) ||
      !identical(observed$metric, reference$metric)) {
    stop("Bootstrap interval identity contract failed.", call. = FALSE)
  }
  difference <- max(abs(observed$low - reference$low), abs(observed$high - reference$high))

  green_reference <- read.csv(file.path(root, "tests", "reference", "green_intervals_5000.csv"))
  green_observed <- intervals[intervals$metric == "green_pct", names(green_reference), drop = FALSE]
  green_observed <- green_observed[order(green_observed$model_id), ]
  green_reference <- green_reference[order(green_reference$model_id), ]
  green_difference <- max(
    abs(green_observed$low - green_reference$low),
    abs(green_observed$high - green_reference$high)
  )
  if (difference > tolerance || green_difference > tolerance) {
    stop(
      "Bootstrap interval equivalence failed: displayed metrics=", format(difference, digits = 17),
      ", Green partition=", format(green_difference, digits = 17),
      call. = FALSE
    )
  }
  c(displayed_metrics = difference, green_partition = green_difference)
}
