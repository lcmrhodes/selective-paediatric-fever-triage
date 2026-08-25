# Step 3: Stage 2 model fitting —-

spot_step_stage2 <- function(state) {
  message("[3/6] Fit and verify the seven Stage 2 strategies")
  state$stage2 <- spot_fit_stage2(
    state$data$stage2_development,
    state$data$validation,
    state$models,
    state$config
  )
  state$frames <- spot_validation_frames(state$stage2)
  saveRDS(
    list(stage2 = state$stage2, frames = state$frames),
    file.path(state$config$paths$derived, "stage2_and_validation.rds")
  )
  state
}
