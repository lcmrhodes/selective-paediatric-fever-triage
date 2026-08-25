#!/usr/bin/env Rscript

# Manifest and required-file checks —-

root <- normalizePath(getwd(), mustWork = TRUE)
source(file.path(root, "R", "model_spec.R"))
source(file.path(root, "R", "prediction.R"))

manifest_path <- file.path(root, "models", "model_bundle_manifest.json")
manifest <- jsonlite::read_json(manifest_path, simplifyVector = FALSE)
if (isTRUE(manifest$participant_rows_included)) {
  stop("The distribution manifest does not pass the participant-row contract.", call. = FALSE)
}

for (entry in manifest$files) {
  path <- file.path(root, "models", entry$file)
  if (!file.exists(path)) stop("Missing model bundle file: ", entry$file, call. = FALSE)
}

# Participant-row safety inspection —-

find_long_vectors <- function(object, path = "preprocessor", limit = 1000L) {
  if (is.atomic(object)) {
    if (length(object) >= limit) return(paste0(path, " [", length(object), "]"))
    return(character())
  }
  if (is.environment(object) || is.function(object) || is.null(object)) return(character())
  if (is.pairlist(object)) object <- as.list(object)
  if (!is.list(object)) return(character())
  child_names <- names(object)
  if (is.null(child_names)) child_names <- as.character(seq_along(object))
  unlist(lapply(seq_along(object), function(index) {
    find_long_vectors(
      object[[index]],
      paste0(path, "$", child_names[[index]]),
      limit
    )
  }), use.names = FALSE)
}

preprocessor <- readRDS(file.path(root, "models", "stage1_preprocessor.rds"))
long_vectors <- find_long_vectors(preprocessor)
if (length(long_vectors)) {
  stop(
    "The distributed preprocessor contains row-length vectors: ",
    paste(long_vectors, collapse = ", "),
    call. = FALSE
  )
}

models <- spot_load_models(root)

# Model identity and embedded-attribute checks —-

if (!identical(names(models$stage2$strategies), spot_stage2_ids)) {
  stop("Stage 2 strategy identity failed.", call. = FALSE)
}
if (length(xgboost::xgb.attributes(models$booster)) > 0L) {
  attributes <- xgboost::xgb.attributes(models$booster)
  prohibited <- intersect(names(attributes), c("label", "weight", "base_margin"))
  if (length(prohibited)) stop("The booster contains prohibited row attributes.", call. = FALSE)
}

# Machine-readable safety report —-

report <- list(
  status = "PASS",
  checked_files = length(manifest$files),
  preprocessor_long_vectors = length(long_vectors),
  stage2_strategies = length(models$stage2$strategies),
  participant_rows_included = FALSE
)
if (dir.exists(file.path(root, "outputs", "verification"))) {
  jsonlite::write_json(
    report,
    file.path(root, "outputs", "verification", "model_bundle_check.json"),
    auto_unbox = TRUE,
    pretty = TRUE
  )
}
print(report)
