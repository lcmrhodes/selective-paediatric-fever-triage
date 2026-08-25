# Step 2: Stage 1 model fitting —-

spot_step_stage1 <- function(state) {
  message("[2/6] Fit and verify Stage 1")
  state$models <- spot_load_models(state$config$root)
  state$stage1 <- spot_fit_stage1(
    state$data$stage1_development,
    state$models,
    state$config
  )
  state
}
