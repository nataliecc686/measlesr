test_that("cases by year summary works", {
  cases_year <- load_data()

  result <- yearly_case_summary(cases_year, country = "Madagascar")

  # check object type
  expect_s3_class(result, "tibble")

  # check expected column names
  expect_equal(names(result), c("year", "total_cases", "pct_change", "trend"))

  # check expected values
  expect_equal(result[1, ], c(2012, 3, 0, "stable"))
  expect_equal(result[5, ], c(2016, 17, 183.3, "surging"))
})
