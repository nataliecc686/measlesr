test_that("plot_monthly_cases works", {
  dat <- load_data("month")
  monthly_cases_plot <- plot_monthly_cases(dat, year = 2018,
                                           country = "Madagascar")
  expect_s3_class(monthly_cases_plot, "ggplot")
})
