#!/usr/bin/env Rscript

# Repository and source loading —-

root <- normalizePath(getwd(), mustWork = TRUE)
if (!file.exists(file.path(root, "DESCRIPTION"))) {
  stop("Run Rscript run_analysis.R from the repository root.", call. = FALSE)
}

for (path in sort(list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE))) {
  source(path)
}
step_paths <- c(
  "analysis/01_data/01_construct_endpoints_and_cohorts.R",
  "analysis/02_stage1/02_fit_stage1.R",
  "analysis/03_stage2/03_fit_stage2.R",
  "analysis/04_validation/04_cambodia_and_bootstrap.R",
  "analysis/05_tables/05_write_tables.R",
  "analysis/06_figures/06_write_figures.R"
)
for (path in file.path(root, step_paths)) source(path)

# Ordered reproduction pipeline —-

state <- list(config = spot_initialize(root))
for (step in list(
  spot_step_data,
  spot_step_stage1,
  spot_step_stage2,
  spot_step_validation,
  spot_step_tables,
  spot_step_figures
)) {
  state <- step(state)
}
report <- spot_write_pipeline_report(state)
# The report is written only after every pipeline step returns successfully.
message(
  "Spot Sepsis Stage 2 reproduction: ", report$status,
  ". Report: outputs/verification/reproduction_report.json"
)
