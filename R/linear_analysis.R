#' Fit a linear regression of log-average measles cases on Air Quality Index
#'
#' Filters to complete AQI and measles case observations, aggregates to
#' country-level averages, log-transforms the outcome, fits a simple linear
#' regression, and returns a formatted \code{gt} table of coefficients.
#'
#' @param data A tibble returned by \code{load_data()}, or any data frame
#'   containing columns \code{country}, \code{total_aqi}, and
#'   \code{total_suspected_measles_rubella_cases}.
#' @param caption A character string for the table caption.
#'   Defaults to \code{"Table 5. Linear Regression coefficients for the
#'   association between measles cases and AQI."}.
#'
#' @return A \code{gt} table object.
#' @export
#'
#' @examples
#' \dontrun{
#'   data <- load_data()
#'   aqi_regression_table(data)
#' }
aqi_regression_table <- function() {

  data <- load_data()

  aqi_model <- data |>
    dplyr::filter(!is.na(total_aqi), !is.na(total_suspected_measles_rubella_cases)) |>
    dplyr::group_by(country, total_aqi) |>
    dplyr::summarise(
      avg_cases = mean(total_suspected_measles_rubella_cases, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(log_avg_cases = log10(avg_cases + 1))

  model <- lm(log_avg_cases ~ total_aqi, data = aqi_model)

  wide_model <- broom::tidy(model, conf.int = TRUE) |>
    dplyr::mutate(
      term = dplyr::recode(term,
                           "(Intercept)" = "Intercept",
                           "total_aqi"   = "Air Quality Index"
      ),
      conf_int = stringr::str_c("(", round(conf.low, 3), ", ", round(conf.high, 3), ")")
    ) |>
    dplyr::select(term, estimate, std.error, statistic, p.value, conf_int) |>
    dplyr::rename(
      Term            = term,
      Estimate        = estimate,
      SE              = std.error,
      Statistic       = statistic,
      "P-value"       = p.value,
      "Conf. Interval" = conf_int
    )

  wide_model |>
    gt::gt() |>
    gt::fmt_number(columns = -"Conf. Interval", decimals = 3) |>
    gt::tab_style(
      style     = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
    ) |>
    gt::tab_style(
      style     = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(
        columns = Term,
        rows    = Term %in% c("Intercept", "Air Quality Index")
      )
    ) |>
    gt::tab_caption(caption = caption)
}
