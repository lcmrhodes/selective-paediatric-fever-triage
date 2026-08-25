# Public model-bundle contents —-

testthat::test_that("distributed model files are listed in their manifest", {
  manifest <- jsonlite::read_json(
    file.path(root, "models", "model_bundle_manifest.json"),
    simplifyVector = FALSE
  )
  testthat::expect_false(isTRUE(manifest$participant_rows_included))
  for (artifact in manifest$files) {
    path <- file.path(root, "models", artifact$file)
    testthat::expect_true(file.exists(path))
    testthat::expect_equal(unname(file.info(path)$size), artifact$bytes)
  }
})

# Fixed Stage 2 identity —-

testthat::test_that("Stage 2 strategy identity is fixed", {
  testthat::expect_identical(names(models$stage2$strategies), spot_stage2_ids)
  testthat::expect_identical(spot_stage2_severe_amber_weight(), 5)
})
