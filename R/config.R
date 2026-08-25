# Repository location —-

spot_find_root <- function(start = getwd()) {
  path <- normalizePath(start, mustWork = TRUE)
  repeat {
    if (file.exists(file.path(path, "DESCRIPTION")) &&
        file.exists(file.path(path, "config", "analysis.yml"))) return(path)
    parent <- dirname(path)
    if (identical(parent, path)) {
      stop("Run this command from the Spot Sepsis Stage 2 repository.", call. = FALSE)
    }
    path <- parent
  }
}

# Configuration loading —-

spot_read_config <- function(root = spot_find_root()) {
  config <- yaml::read_yaml(file.path(root, "config", "analysis.yml"))
  config$root <- root
  config$paths$raw <- file.path(root, config$paths$raw)
  config$paths$derived <- file.path(root, config$paths$derived)
  config$paths$outputs <- file.path(root, config$paths$outputs)
  config
}

# Reproducible runtime initialization —-

spot_initialize <- function(root = spot_find_root()) {
  config <- spot_read_config(root)
  Sys.setenv(SPOT_SEPSIS_ROOT = root)
  set.seed(as.integer(config$analysis$seed))
  RNGkind(sample.kind = "Rejection")
  options(
    stringsAsFactors = FALSE,
    dplyr.summarise.inform = FALSE,
    scipen = 999,
    digits = 15
  )
  dir.create(config$paths$derived, recursive = TRUE, showWarnings = FALSE)
  dir.create(config$paths$outputs, recursive = TRUE, showWarnings = FALSE)
  for (name in c("tables", "figures", "models", "verification", "logs")) {
    dir.create(file.path(config$paths$outputs, name), recursive = TRUE, showWarnings = FALSE)
  }
  config
}
