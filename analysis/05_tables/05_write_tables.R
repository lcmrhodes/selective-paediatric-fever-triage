# Step 5: manuscript tables —-

spot_step_tables <- function(state) {
  message("[5/6] Reproduce manuscript and supplementary tables")
  state$table_result <- spot_write_tables(
    state$data,
    state$stage2,
    state$frames,
    state$intervals,
    state$config
  )
  state$table_check <- spot_check_display_tables(
    state$table_result,
    state$config$root
  )
  state
}
