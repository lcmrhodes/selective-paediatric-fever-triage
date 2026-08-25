# Authorized-data reproduction invariants —-

testthat::test_that("complete reproduction report passes when generated", {
  path <- file.path(root, "outputs", "verification", "reproduction_report.json")
  testthat::skip_if_not(file.exists(path), "Run Rscript run_analysis.R with authorized data first.")
  report <- jsonlite::read_json(path, simplifyVector = TRUE)
  testthat::expect_identical(report$status, "PASS")
  testthat::expect_identical(report$cohort_fingerprint$stage1_rows, 2578L)
  testthat::expect_identical(report$cohort_fingerprint$stage1_events, 92L)
  testthat::expect_identical(report$stage2$strategy_count, 7L)
  testthat::expect_identical(report$tables$reviewed_primary_displayed_cells, 35L)
})
