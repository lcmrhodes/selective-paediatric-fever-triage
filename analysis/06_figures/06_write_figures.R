# Step 6: manuscript figures —-

spot_step_figures <- function(state) {
  message("[6/6] Reproduce manuscript and supplementary figures")
  state$figure_result <- spot_write_figures(
    state$frames,
    state$intervals,
    state$config
  )
  state$figure_check <- spot_check_figure_sources(
    state$figure_result,
    state$config$root
  )
  state
}
