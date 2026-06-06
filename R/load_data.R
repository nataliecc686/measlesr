#' Load measles dataset
#'
#' @return A tibble containing measles data
#' @export
load_data <- function() {
  path <- system.file("extdata", "cases_month.parquet", package = "measlesr")

  if (path == "") {
    stop("Data file not found. Make sure the package is installed correctly")
  }

  arrow::read_parquet(path)
}
