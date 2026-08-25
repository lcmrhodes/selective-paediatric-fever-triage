#!/usr/bin/env Rscript

# Browser-model export contract —-

suppressPackageStartupMessages({
  library(jsonlite)
  library(xgboost)
})

args <- commandArgs(trailingOnly = TRUE)
output_path <- if (length(args)) args[[1]] else file.path("models", "browser_model_bundle.json")
root <- normalizePath(getwd(), mustWork = TRUE)

source(file.path(root, "R", "model_spec.R"), local = TRUE)

required <- c(
  file.path(root, "models", "stage1_preprocessor.rds"),
  file.path(root, "models", "stage1_booster.ubj"),
  file.path(root, "models", "stage1_calibration.json"),
  file.path(root, "models", "stage2_models.json")
)
missing <- required[!file.exists(required)]
if (length(missing)) {
  stop("Prediction artifact missing: ", paste(basename(missing), collapse = ", "), call. = FALSE)
}

# Bagged-imputation tree conversion —-

export_rpart_split <- function(split_matrix, index) {
  row <- split_matrix[index, , drop = FALSE]
  ncat <- as.integer(row[1, "ncat"])
  if (!ncat %in% c(-1L, 1L)) {
    stop("The browser exporter supports only numeric rpart splits.", call. = FALSE)
  }
  list(
    feature = rownames(split_matrix)[[index]],
    threshold = unname(as.numeric(row[1, "index"])),
    less_than_goes_left = identical(ncat, -1L)
  )
}

export_rpart_tree <- function(tree) {
  frame <- tree$frame
  split_matrix <- tree$splits
  node_ids <- as.integer(rownames(frame))
  nodes <- vector("list", nrow(frame))
  split_cursor <- 1L

  for (index in seq_len(nrow(frame))) {
    node_id <- node_ids[[index]]
    if (identical(as.character(frame$var[[index]]), "<leaf>")) {
      nodes[[index]] <- list(id = node_id, value = unname(as.numeric(frame$yval[[index]])))
      next
    }

    competitor_count <- as.integer(frame$ncompete[[index]])
    surrogate_count <- as.integer(frame$nsurrogate[[index]])
    primary <- export_rpart_split(split_matrix, split_cursor)
    surrogate_start <- split_cursor + 1L + competitor_count
    surrogate_indices <- if (surrogate_count) {
      seq.int(surrogate_start, length.out = surrogate_count)
    } else {
      integer()
    }
    surrogates <- lapply(surrogate_indices, function(row_index) {
      export_rpart_split(split_matrix, row_index)
    })

    left_index <- match(node_id * 2L, node_ids)
    right_index <- match(node_id * 2L + 1L, node_ids)
    if (is.na(left_index) || is.na(right_index)) {
      stop("A fitted imputation tree has an incomplete child pair.", call. = FALSE)
    }

    nodes[[index]] <- list(
      id = node_id,
      value = unname(as.numeric(frame$yval[[index]])),
      primary = primary,
      surrogates = surrogates,
      left = node_id * 2L,
      right = node_id * 2L + 1L,
      majority = if (frame$wt[[left_index]] >= frame$wt[[right_index]]) "left" else "right"
    )
    split_cursor <- split_cursor + 1L + competitor_count + surrogate_count
  }

  consumed_splits <- if (is.null(split_matrix)) 0L else nrow(split_matrix)
  if (split_cursor - 1L != consumed_splits) {
    stop("The fitted imputation-tree split layout was not fully consumed.", call. = FALSE)
  }

  list(
    use_surrogates = as.integer(tree$control$usesurrogate),
    nodes = nodes
  )
}

preprocessor <- readRDS(required[[1]])
imputation_step <- which(vapply(preprocessor$steps, inherits, logical(1), "step_impute_bag"))
if (length(imputation_step) != 1L) {
  stop("Unexpected Stage 1 preprocessing contract.", call. = FALSE)
}
imputation_models <- preprocessor$steps[[imputation_step]]$models

imputers <- lapply(names(imputation_models), function(target) {
  model <- imputation_models[[target]]
  list(
    target = target,
    predictors = unname(model$..imp_vars),
    aggregation = "average",
    cast = if (is.integer(model$y) && !is.factor(model$y)) "rounded_integer" else "numeric",
    trees = lapply(model$mtrees, function(tree) export_rpart_tree(tree$btree))
  )
})
names(imputers) <- names(imputation_models)

# XGBoost model conversion —-

booster <- xgboost::xgb.load(required[[2]])
temporary_json <- tempfile(fileext = ".json")
on.exit(unlink(temporary_json), add = TRUE)
invisible(xgboost::xgb.save(booster, temporary_json))
saved_model <- jsonlite::read_json(temporary_json, simplifyVector = FALSE)
learner <- saved_model$learner
tree_model <- learner$gradient_booster$model
if (length(tree_model$trees) != 5000L) {
  stop("The fixed Stage 1 tree count changed.", call. = FALSE)
}

trees <- lapply(tree_model$trees, function(tree) {
  list(
    left = unlist(tree$left_children, use.names = FALSE),
    right = unlist(tree$right_children, use.names = FALSE),
    default_left = unlist(tree$default_left, use.names = FALSE),
    split_index = unlist(tree$split_indices, use.names = FALSE),
    split_condition = unlist(tree$split_conditions, use.names = FALSE)
  )
})

calibration <- jsonlite::read_json(required[[3]], simplifyVector = FALSE)
stage2 <- jsonlite::read_json(required[[4]], simplifyVector = FALSE)

# WHO weight-for-age reference conversion —-

if (!requireNamespace("zscorer", quietly = TRUE)) {
  stop("The pinned zscorer package is required to export the weight reference.", call. = FALSE)
}
growth_data <- get("wgsrData", envir = asNamespace("zscorer"))
weight_reference <- growth_data[growth_data$index == "wfa", c("sex", "given", "l", "m", "s")]
weight_reference <- weight_reference[order(weight_reference$sex, weight_reference$given), ]

growth_by_sex <- lapply(c(1L, 2L), function(sex) {
  rows <- weight_reference$sex == sex
  list(
    age_days = unname(weight_reference$given[rows]),
    l = unname(weight_reference$l[rows]),
    m = unname(weight_reference$m[rows]),
    s = unname(weight_reference$s[rows])
  )
})
names(growth_by_sex) <- c("male", "female")

# Versioned browser-neutral bundle —-

bundle <- list(
  schema_version = 1L,
  model_version = "2026-08-24",
  participant_rows_included = FALSE,
  stage1 = list(
    predictors = unname(spot_stage1_predictors),
    features = unname(spot_stage1_features),
    missing_indicator_prefix = "na_ind_",
    preprocessing = list(imputers = imputers),
    booster = list(
      objective = learner$objective$name,
      base_score = as.numeric(learner$learner_model_param$base_score),
      output = "complement",
      trees = trees
    ),
    calibration = calibration,
    thresholds = list(green_upper_exclusive = 0.005, amber_upper_inclusive = 0.02)
  ),
  stage2 = stage2,
  weight_for_age = list(
    reference = "WHO Child Growth Standards 2006",
    source_implementation = "zscorer 0.3.1",
    age_months_minimum = 1L,
    age_months_maximum = 59L,
    reference_by_sex = growth_by_sex
  )
)

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  bundle,
  output_path,
  auto_unbox = TRUE,
  pretty = FALSE,
  digits = 17,
  null = "null"
)
message("Wrote browser model bundle to ", normalizePath(output_path))
