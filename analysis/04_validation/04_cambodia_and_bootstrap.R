# Step 4: held-out validation and intervals —-

spot_step_validation <- function(state) {
  message("[4/6] Evaluate held-out Cambodia data and calculate bootstrap intervals")
  state$intervals <- spot_bootstrap_intervals(state$frames, state$config)
  state$bootstrap_check <- spot_check_bootstrap(
    state$intervals,
    state$config$root
  )
  saveRDS(
    state$intervals,
    file.path(state$config$paths$derived, "bootstrap_intervals.rds")
  )
  readr::write_csv(
    state$intervals,
    file.path(state$config$paths$outputs, "verification", "bootstrap_intervals.csv")
  )
  state
}
