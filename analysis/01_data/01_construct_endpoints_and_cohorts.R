# Step 1: endpoints and cohorts —-

spot_step_data <- function(state) {
  message("[1/6] Construct endpoints and analysis cohorts")
  state$data <- spot_load_analysis_data(state$config)
  saveRDS(state$data, file.path(state$config$paths$derived, "analysis_data.rds"))
  state
}
