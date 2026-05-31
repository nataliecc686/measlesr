#' Create a line plot of measles cases for a country in a given year
#'
#' @param data.frame cases dataset
#' @param numerical year
#' @param string country
#'
#' @return ggplot
#'
#' @examples
#' plot_monthly_cases(cases_month, year = 2018, country = "Madagascar)
#'
#' @export

plot_monthly_cases <- function(cases_month, year = 2018, country = "Madagascar") {

  plot_data <- filter_by_country(cases_month, country)  # <-- use helper
  plot_data <- plot_data[plot_data$year == year, ]

  if (nrow(plot_data) == 0) {
    stop("No data found for year = ", year, ".")
  }

  plot_data$month <- factor(plot_data$month,
                            levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                       "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

  ggplot2::ggplot(plot_data, ggplot2::aes(x = month, y = cases, group = 1)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(
      title = paste("Monthly Measles Cases -", country, year),
      x     = "Month",
      y     = "Total Cases"
    ) +
    ggplot2::theme_minimal()
}
