# Shared test fixtures and model bundle —-

root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)
Sys.setenv(SPOT_SEPSIS_ROOT = root)
source(file.path(root, "R", "model_spec.R"))
source(file.path(root, "R", "prediction.R"))
fixtures <- jsonlite::read_json(
  file.path(root, "tests", "fixtures", "prediction_fixtures.json"),
  simplifyVector = FALSE
)
models <- spot_load_models(root)
