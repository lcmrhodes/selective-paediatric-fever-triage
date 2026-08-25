# Shared figure palette —-

spot_figure_palette <- c(Green = "#2CA25F", Amber = "#FDAE61", Red = "#D73027")

# Figure source-data preparation —-

spot_calibration_bins <- function(frames) {
  dplyr::bind_rows(lapply(names(frames), function(id) {
    frame <- frames[[id]]
    cuts <- unique(stats::quantile(
      frame$cascade_pred,
      probs = seq(0, 1, 0.1),
      na.rm = TRUE,
      type = 2
    ))
    if (length(cuts) < 3L) return(data.frame())
    frame$bin <- cut(
      frame$cascade_pred,
      breaks = cuts,
      include.lowest = TRUE,
      labels = FALSE
    )
    frame |>
      dplyr::group_by(bin) |>
      dplyr::summarise(
        mean_predicted = stats::weighted.mean(cascade_pred, sampling_weight),
        observed = stats::weighted.mean(outcome.binary_num, sampling_weight),
        .groups = "drop"
      ) |>
      dplyr::mutate(model_id = id, Strategy = unname(spot_labels()[[id]]), .before = 1)
  }))
}

spot_amber_redistribution_data <- function(frames) {
  ids <- c("spo2", "strem1", "crp_strem1", "crp_strem1_glucose")
  titles <- c(
    spo2 = "Oxygen saturation",
    strem1 = "sTREM-1",
    crp_strem1 = "CRP + sTREM-1",
    crp_strem1_glucose = "CRP + sTREM-1 + glucose"
  )
  scientific_names <- c(
    spo2 = "S1 clinical-only -> S2 SpO2",
    strem1 = "S1 clinical-only -> S2 sTREM-1",
    crp_strem1 = "S1 clinical-only -> S2 CRP + sTREM-1",
    crp_strem1_glucose = "S1 clinical-only -> S2 CRP + sTREM-1 + glucose"
  )
  base <- frames$s1_baseline
  total_weight <- sum(base$sampling_weight)
  amber <- base$s1_disposition == "AMBER"
  amber_weight <- sum(base$sampling_weight[amber])
  amber_per_10000 <- round(amber_weight / total_weight * 10000)

  dplyr::bind_rows(lapply(ids, function(id) {
    frame <- frames[[id]]
    severe <- frame$outcome.binary_num == 1
    dplyr::bind_rows(lapply(c("GREEN", "AMBER", "RED"), function(zone) {
      selected <- amber & frame$cascade_disposition == zone
      count_per_10000 <- round(sum(frame$sampling_weight[selected]) / total_weight * 10000)
      severe_per_10000 <- round(sum(frame$sampling_weight[selected & severe]) / total_weight * 10000)
      nonsevere_per_10000 <- round(sum(frame$sampling_weight[selected & !severe]) / total_weight * 10000)
      data.frame(
        panel = unname(titles[[id]]),
        model_id = id,
        stage2_strategy = unname(scientific_names[[id]]),
        disposition = tools::toTitleCase(tolower(zone)),
        weighted_percent_of_s1_amber = round(sum(frame$sampling_weight[selected]) / amber_weight * 100, 2),
        projected_per_10000 = count_per_10000,
        severe_per_10000 = severe_per_10000,
        nonsevere_per_10000 = nonsevere_per_10000,
        stage1_amber_per_10000 = amber_per_10000,
        stringsAsFactors = FALSE
      )
    }))
  }))
}

spot_severe_destination_data <- function(frames, intervals) {
  ids <- c("s1_baseline", "spo2", "strem1", "crp_strem1", "crp_strem1_glucose")
  dplyr::bind_rows(lapply(ids, function(id) {
    frame <- frames[[id]]
    severe <- frame$outcome.binary_num == 1
    all <- rep(TRUE, nrow(frame))
    weight <- frame$sampling_weight
    red_interval <- spot_interval(intervals, id, "severe_red_pop_pct")
    data.frame(
      model_id = id,
      Strategy = unname(spot_labels()[[id]]),
      Green = spot_weighted_percent(severe & frame$cascade_disposition == "GREEN", all, weight) * 100,
      Amber = spot_weighted_percent(severe & frame$cascade_disposition == "AMBER", all, weight) * 100,
      Red = spot_weighted_percent(severe & frame$cascade_disposition == "RED", all, weight) * 100,
      Red_low = red_interval[["low"]] * 100,
      Red_high = red_interval[["high"]] * 100,
      stringsAsFactors = FALSE
    )
  }))
}

# Flow-geometry helpers —-

spot_curve_points <- function(x0, y0, x1, y1, n = 90, bend = 0.44) {
  value <- seq(0, 1, length.out = n)
  cx0 <- x0 + (x1 - x0) * bend
  cx1 <- x1 - (x1 - x0) * bend
  data.frame(
    x = ((1 - value)^3 * x0) + (3 * (1 - value)^2 * value * cx0) +
      (3 * (1 - value) * value^2 * cx1) + (value^3 * x1),
    y = ((1 - value)^3 * y0) + (3 * (1 - value)^2 * value * y0) +
      (3 * (1 - value) * value^2 * y1) + (value^3 * y1)
  )
}

spot_ribbon_between <- function(x0, y0, x1, y1, width0, width1) {
  dplyr::bind_rows(
    spot_curve_points(x0, y0 + width0 / 2, x1, y1 + width1 / 2),
    spot_curve_points(x1, y1 - width1 / 2, x0, y0 - width0 / 2)
  )
}

# Manuscript flow figures —-

spot_amber_redistribution_plot <- function(flow) {
  panel_levels <- c(
    "Oxygen saturation", "sTREM-1",
    "CRP + sTREM-1", "CRP + sTREM-1 + glucose"
  )
  flow$panel <- factor(flow$panel, levels = panel_levels)
  height <- 0.56
  source_center <- 0.50
  source_x <- 0.20
  terminal_x <- 0.66
  half_width <- 0.022
  terminal_center <- c(Green = 0.80, Amber = 0.50, Red = 0.20)

  # Build nodes and ribbons separately for each reported panel.
  panels <- lapply(split(flow, flow$panel), function(rows) {
    rows <- rows[match(c("Green", "Amber", "Red"), rows$disposition), ]
    fraction <- rows$weighted_percent_of_s1_amber / 100
    names(fraction) <- rows$disposition
    top <- source_center + height / 2
    source_subcenter <- c(
      Green = top - fraction[["Green"]] * height / 2,
      Amber = top - (fraction[["Green"]] + fraction[["Amber"]] / 2) * height,
      Red = top - (fraction[["Green"]] + fraction[["Amber"]] + fraction[["Red"]] / 2) * height
    )
    source_node <- data.frame(
      panel = rows$panel[[1]], disposition = "Source",
      xmin = source_x - half_width, xmax = source_x + half_width,
      ymin = source_center - height / 2, ymax = source_center + height / 2
    )
    terminal_nodes <- data.frame(
      panel = rows$panel[[1]], disposition = rows$disposition,
      xmin = terminal_x - half_width, xmax = terminal_x + half_width,
      ymin = terminal_center[rows$disposition] - fraction * height / 2,
      ymax = terminal_center[rows$disposition] + fraction * height / 2
    )
    ribbons <- dplyr::bind_rows(lapply(rows$disposition, function(disposition) {
      width <- fraction[[disposition]] * height
      if (width <= 1e-6) return(NULL)
      spot_ribbon_between(
        source_x + half_width, source_subcenter[[disposition]],
        terminal_x - half_width, terminal_center[[disposition]],
        width, width
      ) |>
        dplyr::mutate(
          panel = rows$panel[[1]],
          disposition = disposition,
          flow_id = paste(rows$panel[[1]], disposition)
        )
    }))
    list(nodes = dplyr::bind_rows(source_node, terminal_nodes), ribbons = ribbons)
  })
  nodes <- dplyr::bind_rows(lapply(panels, `[[`, "nodes"))
  ribbons <- dplyr::bind_rows(lapply(panels, `[[`, "ribbons"))

  # Add projected counts and severe/non-severe labels.
  annotations <- flow |>
    dplyr::mutate(
      x = terminal_x + half_width + 0.015,
      y = terminal_center[disposition],
      value_label = paste0(disposition, ": ", format(projected_per_10000, big.mark = ","), " / 10,000"),
      severity_label = paste0(
        format(severe_per_10000, big.mark = ","), " severe · ",
        format(nonsevere_per_10000, big.mark = ","), " non-severe"
      )
    )
  source_labels <- unique(flow[c("panel", "stage1_amber_per_10000")]) |>
    dplyr::mutate(
      x = source_x,
      y = source_center + height / 2 + 0.11,
      label = paste0("Stage 2\n(Stage 1 Amber)\n", format(stage1_amber_per_10000, big.mark = ","), " / 10,000")
  )
  node_colours <- c(Source = "#BDBDBD", spot_figure_palette)

  # Assemble the final faceted flow diagram.
  ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = ribbons,
      ggplot2::aes(x = x, y = y, group = flow_id, fill = disposition),
      color = NA, alpha = 0.78
    ) +
    ggplot2::geom_rect(
      data = nodes,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = disposition),
      color = "#3A3A3A", linewidth = 0.3
    ) +
    ggplot2::geom_text(
      data = source_labels,
      ggplot2::aes(x = x, y = y, label = label),
      size = 2.7, lineheight = 0.95, color = "#333333"
    ) +
    ggplot2::geom_text(
      data = annotations,
      ggplot2::aes(x = x, y = y + 0.045, label = value_label),
      size = 2.9, hjust = 0, fontface = "bold", color = "#222222"
    ) +
    ggplot2::geom_text(
      data = annotations,
      ggplot2::aes(x = x, y = y - 0.005, label = severity_label),
      size = 2.5, hjust = 0, color = "#4A4A4A"
    ) +
    ggplot2::scale_fill_manual(
      values = node_colours,
      breaks = c("Green", "Amber", "Red"),
      labels = c("Returned to Green", "Retained Amber", "Escalated to Red"),
      name = NULL
    ) +
    ggplot2::facet_wrap(~panel, ncol = 2) +
    ggplot2::coord_cartesian(xlim = c(0.02, 1.06), ylim = c(0.02, 0.98), expand = FALSE, clip = "off") +
    ggplot2::theme_void(base_size = 10) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      strip.text = ggplot2::element_text(face = "bold", size = 11, color = "#222222"),
      legend.position = "top",
      panel.spacing = grid::unit(1.4, "lines"),
      plot.margin = ggplot2::margin(14, 18, 14, 18)
    )
}

spot_severe_destination_plot <- function(source) {
  strategy_levels <- rev(source$Strategy)

  # Prepare the stacked disposition panel.
  bars <- tidyr::pivot_longer(
    source,
    c("Green", "Amber", "Red"),
    names_to = "Disposition",
    values_to = "Projected"
  ) |>
    dplyr::mutate(
      Strategy = factor(Strategy, levels = strategy_levels),
      Disposition = factor(Disposition, levels = c("Green", "Amber", "Red")),
      Panel = "Severe-child disposition (per 10,000)"
    )
  # Prepare the Red-classification estimate and interval panel.
  forest <- source |>
    dplyr::mutate(
      Strategy = factor(Strategy, levels = strategy_levels),
      Panel = "Severe referred to Red (95% CI)"
    )
  panels <- c("Severe-child disposition (per 10,000)", "Severe referred to Red (95% CI)")
  bars$Panel <- factor(bars$Panel, levels = panels)
  forest$Panel <- factor(forest$Panel, levels = panels)

  ggplot2::ggplot() +
    ggplot2::geom_col(
      data = bars,
      ggplot2::aes(Projected, Strategy, fill = Disposition),
      width = 0.62, colour = "white", linewidth = 0.25,
      position = ggplot2::position_stack(reverse = TRUE)
    ) +
    ggplot2::geom_text(
      data = bars,
      ggplot2::aes(Projected, Strategy, label = ifelse(Projected >= 1.5, round(Projected), "")),
      position = ggplot2::position_stack(vjust = 0.5, reverse = TRUE),
      size = 3.0, colour = "grey15"
    ) +
    ggplot2::geom_errorbar(
      data = forest,
      ggplot2::aes(y = Strategy, xmin = Red_low, xmax = Red_high),
      orientation = "y", width = 0.18, linewidth = 0.6, colour = "grey45"
    ) +
    ggplot2::geom_point(
      data = forest,
      ggplot2::aes(Red, Strategy),
      shape = 21, size = 3.2, stroke = 0.6, fill = "#C9CCD1", colour = "grey20"
    ) +
    ggplot2::scale_fill_manual(values = spot_figure_palette, name = NULL) +
    ggplot2::facet_grid(. ~ Panel, scales = "free_x", space = "free_x") +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 10.5) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(colour = "grey70", fill = NA, linewidth = 0.5),
      strip.text = ggplot2::element_text(face = "bold", size = 9.2),
      legend.position = "top",
      panel.spacing = grid::unit(1.2, "lines"),
      plot.margin = ggplot2::margin(10, 16, 10, 10)
    )
}

# Participant-flow and calibration figures —-

spot_participant_flow_plot <- function() {
  ggplot2::ggplot() +
    ggplot2::annotate("rect", xmin = 0.15, xmax = 0.85, ymin = 0.82, ymax = 0.98, fill = "#E8F1F5", colour = "#0F506B") +
    ggplot2::annotate("text", x = 0.5, y = 0.90, label = "Parent cohort\nn=3,423", fontface = "bold", size = 4.2) +
    ggplot2::annotate("segment", x = 0.5, xend = 0.5, y = 0.82, yend = 0.70, arrow = grid::arrow(length = grid::unit(0.12, "in")), colour = "grey35") +
    ggplot2::annotate("text", x = 0.72, y = 0.76, label = "18 without parent-study endpoint", hjust = 0, size = 3.1, colour = "grey30") +
    ggplot2::annotate("rect", xmin = 0.15, xmax = 0.85, ymin = 0.54, ymax = 0.70, fill = "#E8F1F5", colour = "#0F506B") +
    ggplot2::annotate("text", x = 0.5, y = 0.62, label = "Paper analysis cohort\nn=3,405; 133 severe", fontface = "bold", size = 4.0) +
    ggplot2::annotate("segment", x = 0.5, xend = 0.27, y = 0.54, yend = 0.40, arrow = grid::arrow(length = grid::unit(0.12, "in")), colour = "grey35") +
    ggplot2::annotate("segment", x = 0.5, xend = 0.73, y = 0.54, yend = 0.40, arrow = grid::arrow(length = grid::unit(0.12, "in")), colour = "grey35") +
    ggplot2::annotate("rect", xmin = 0.05, xmax = 0.47, ymin = 0.18, ymax = 0.40, fill = "#F7F7F7", colour = "#0F506B") +
    ggplot2::annotate("text", x = 0.26, y = 0.29, label = "Derivation sites\nStage 2: n=2,581; 97 severe\nStage 1 contract:\nn=2,578; 92 events", size = 3.55) +
    ggplot2::annotate("rect", xmin = 0.53, xmax = 0.95, ymin = 0.18, ymax = 0.40, fill = "#F7F7F7", colour = "#0F506B") +
    ggplot2::annotate("text", x = 0.74, y = 0.29, label = "Held-out Cambodia\nn=824; 36 severe\nScored after model and\nreporting choices were fixed", size = 3.55) +
    ggplot2::annotate("text", x = 0.5, y = 0.07, label = "Stage 1 used death or organ support within 2 days; Stage 2 used the parent-study severe-illness endpoint.", size = 3.25, colour = "grey25") +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA))
}

spot_calibration_plot <- function(source) {
  ggplot2::ggplot(source, ggplot2::aes(mean_predicted, observed)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "grey55") +
    ggplot2::geom_line(colour = "#0F506B", linewidth = 0.55) +
    ggplot2::geom_point(colour = "#0F506B", size = 1.5) +
    ggplot2::facet_wrap(~Strategy, scales = "free", ncol = 2) +
    ggplot2::scale_x_continuous(labels = scales::label_percent(accuracy = 0.1)) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 0.1)) +
    ggplot2::labs(x = "Mean predicted risk", y = "Weighted observed event proportion") +
    ggplot2::theme_minimal(base_size = 9) +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold", size = 8),
      panel.grid.minor = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA)
    )
}

# Figure export and regression checks —-

spot_save_plot <- function(plot, path_stub, width, height) {
  ggplot2::ggsave(paste0(path_stub, ".png"), plot = plot, width = width, height = height, units = "in", dpi = 300, bg = "white")
  ggplot2::ggsave(paste0(path_stub, ".pdf"), plot = plot, width = width, height = height, units = "in", device = grDevices::cairo_pdf, bg = "white")
}

spot_write_figures <- function(frames, intervals, config) {
  output <- file.path(config$paths$outputs, "figures")
  flow <- spot_amber_redistribution_data(frames)
  severe <- spot_severe_destination_data(frames, intervals)
  calibration <- spot_calibration_bins(frames)
  readr::write_csv(flow, file.path(output, "figure_1_source.csv"))
  readr::write_csv(severe, file.path(output, "figure_2_source.csv"))
  readr::write_csv(calibration, file.path(output, "supp_figure_2_source.csv"))
  readr::write_csv(
    data.frame(
      cohort = c("Parent cohort", "Paper analysis cohort", "Stage 1 development", "Stage 2 derivation", "Cambodia validation"),
      rows = c(3423L, 3405L, 2578L, 2581L, 824L),
      events = c(133L, 133L, 92L, 97L, 36L)
    ),
    file.path(output, "supp_figure_1_source.csv")
  )
  spot_save_plot(spot_amber_redistribution_plot(flow), file.path(output, "figure_1_amber_redistribution"), 9.6, 7.4)
  spot_save_plot(spot_severe_destination_plot(severe), file.path(output, "figure_2_severe_destinations"), 10.4, 4.6)
  spot_save_plot(spot_participant_flow_plot(), file.path(output, "supp_figure_1_participant_flow"), 7.2, 6.6)
  spot_save_plot(spot_calibration_plot(calibration), file.path(output, "supp_figure_2_validation_calibration"), 7.2, 8.5)
  list(flow = flow, severe = severe, calibration = calibration)
}

spot_check_figure_sources <- function(sources, root, tolerance = 5e-12) {
  flow_reference <- readr::read_csv(
    file.path(root, "tests", "reference", "figure_1_source.csv"),
    show_col_types = FALSE
  )
  flow_observed <- sources$flow[, c(
    "panel", "stage2_strategy", "disposition", "weighted_percent_of_s1_amber",
    "projected_per_10000", "severe_per_10000", "nonsevere_per_10000"
  )]
  if (!identical(as.data.frame(flow_observed), as.data.frame(flow_reference))) {
    stop("Figure 1 source-value equivalence failed.", call. = FALSE)
  }

  severe_reference <- readr::read_csv(
    file.path(root, "tests", "reference", "figure_2_source.csv"),
    show_col_types = FALSE
  )
  labels <- c(
    s1_baseline = "Clinical-only S1",
    spo2 = "+ SpO2",
    strem1 = "+ sTREM-1",
    crp_strem1 = "+ CRP + sTREM-1",
    crp_strem1_glucose = "+ CRP + sTREM-1 + glucose"
  )
  severe_observed <- data.frame(
    strategy = unname(labels[sources$severe$model_id]),
    severe_green_missed = sources$severe$Green,
    severe_amber_monitored = sources$severe$Amber,
    severe_red_referred = sources$severe$Red,
    severe_red_ci_low = sources$severe$Red_low,
    severe_red_ci_high = sources$severe$Red_high,
    total_severe_per_10000 = sources$severe$Green + sources$severe$Amber + sources$severe$Red,
    stringsAsFactors = FALSE
  )
  point_columns <- c(
    "severe_green_missed", "severe_amber_monitored",
    "severe_red_referred", "total_severe_per_10000"
  )
  severe_observed[point_columns] <- lapply(severe_observed[point_columns], round, digits = 0)
  severe_observed[c("severe_red_ci_low", "severe_red_ci_high")] <- lapply(
    severe_observed[c("severe_red_ci_low", "severe_red_ci_high")],
    round,
    digits = 1
  )
  if (!identical(severe_observed$strategy, severe_reference$strategy)) {
    stop("Figure 2 strategy identity contract failed.", call. = FALSE)
  }
  numeric_columns <- setdiff(names(severe_reference), "strategy")
  difference <- max(abs(as.matrix(severe_observed[numeric_columns]) - as.matrix(severe_reference[numeric_columns])))
  if (difference > tolerance) stop("Figure 2 source-value equivalence failed.", call. = FALSE)
  c(figure_1 = 0, figure_2 = difference)
}
