library(testthat)
library(grainkey)

test_that("build_grain_key returns expected structure", {
  dummy_data <- tibble::tibble(
    operation_date = as.Date(c("2025-01-01", "2025-01-01")),
    edad = c(0, 1),
    ptot_g = c(8.5, 15.2)
  )

  result <- build_grain_key(dummy_data)

  expect_type(result, "list")
  expect_true(all(c("grain_key", "weight_key", "long_data") %in% names(result)))
  expect_s3_class(result$grain_key, "data.frame")
  expect_s3_class(result$weight_key, "data.frame")
  expect_s3_class(result$long_data, "data.frame")
})
