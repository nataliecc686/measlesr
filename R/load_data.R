#' Load Global Measles Case Data
#'
#' Downloads and returns one or both of the WHO measles datasets from the
#' Tidy Tuesday GitHub repository (2025-06-24 release). Two datasets are
#' available: monthly case counts (`"month"`) and annual case counts
#' (`"year"`).
#'
#' @param dataset Character string specifying which dataset to load.
#'   One of `"month"` (default), `"year"`, or `"both"`. When `"both"` is
#'   chosen, a named list with elements `cases_month` and `cases_year` is
#'   returned.
#'
#' @return When `dataset` is `"month"` or `"year"`, a tibble. When
#'   `dataset` is `"both"`, a named list of two tibbles:
#'   \describe{
#'     \item{cases_month}{Monthly case counts per country. Key columns:
#'       `country`, `year`, `month`, `measles_total`.}
#'     \item{cases_year}{Annual case counts per country. Key columns:
#'       `country`, `year`, `measles_total`, `measles_clinical`,
#'       `measles_epi_linked`, `measles_lab_confirmed`,
#'       `total_population`.}
#'   }
#'
#' @examples
#' \dontrun{
#'   # Monthly data only (default)
#'   cases_month <- load_data()
#'
#'   # Annual data
#'   cases_year <- load_data("year")
#'
#'   # Both at once
#'   dat <- load_data("both")
#'   cases_month <- dat$cases_month
#'   cases_year  <- dat$cases_year
#' }
#'
#' @importFrom readr read_csv
#' @export
load_data <- function(dataset = "month") {
  base_url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-24"

  if (!dataset %in% c("month", "year", "both")) {
    stop('`dataset` must be "month", "year", or "both".')
  }

  read <- function(name) {
    readr::read_csv(
      paste0(base_url, "/cases_", name, ".csv"),
      show_col_types = FALSE
    )
  }

  if (dataset == "month") return(read("month"))
  if (dataset == "year")  return(read("year"))

  list(
    cases_month = read("month"),
    cases_year  = read("year")
  )
}
