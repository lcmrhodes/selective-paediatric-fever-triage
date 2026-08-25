# Stage 1 fixed fixtures —-

testthat::test_that("Stage 1 fixed fixtures match", {
  for (fixture in fixtures$stage1) {
    input <- as.data.frame(
      lapply(fixture$input, function(value) if (is.null(value)) NA_real_ else value),
      check.names = FALSE
    )
    result <- spot_predict_stage1(input, models)
    testthat::expect_equal(
      result$raw_probability[[1]],
      fixture$raw_probability,
      tolerance = 1e-12,
      info = fixture$id
    )
    testthat::expect_equal(
      result$probability[[1]],
      fixture$probability,
      tolerance = 1e-12,
      info = fixture$id
    )
    testthat::expect_identical(result$classification[[1]], fixture$classification)
  }
})

# Stage 2 fixed fixtures —-

testthat::test_that("all seven Stage 2 strategies match fixed fixtures", {
  for (expected in fixtures$stage2$expected) {
    result <- spot_predict_stage2(
      fixtures$stage2$stage1_probability,
      expected$strategy,
      fixtures$stage2$measures,
      models
    )
    testthat::expect_equal(result$probability, expected$probability, tolerance = 1e-12)
    testthat::expect_identical(result$classification, expected$classification)
  }
})

# Missing-value behavior —-

testthat::test_that("fixed missing-value behavior matches", {
  fixture <- fixtures$stage2_missing
  result <- spot_predict_stage2(
    fixture$stage1_probability,
    fixture$strategy,
    fixture$measures,
    models
  )
  testthat::expect_equal(result$probability, fixture$probability, tolerance = 1e-12)
  testthat::expect_identical(result$classification, fixture$classification)
  testthat::expect_identical(result$imputed, unlist(fixture$imputed, use.names = FALSE))
})

# Traffic-light boundaries —-

testthat::test_that("traffic-light boundaries are exact", {
  for (fixture in fixtures$thresholds) {
    testthat::expect_identical(
      unname(spot_zone(fixture$probability)),
      fixture$classification
    )
  }
})

# Safety and categorical edge cases —-

testthat::test_that("categorical edge cases fail closed", {
  input <- as.data.frame(fixtures$stage1[[1]]$input, check.names = FALSE)
  input$sex <- 2
  testthat::expect_error(
    spot_predict_stage1(input, models),
    "Binary Stage 1 fields"
  )
})

testthat::test_that("Stage 2 cannot alter final Stage 1 dispositions", {
  testthat::expect_error(
    spot_predict_stage2(0.004, "spo2", list(spo2 = 98), models),
    "only after an Amber"
  )
  testthat::expect_error(
    spot_predict_stage2(0.03, "spo2", list(spo2 = 98), models),
    "only after an Amber"
  )
})
