#' Filter measles dataset by country
#'
#' @param tibble dataset
#' @param string country to be filtered by
#'
#' @return tibble
#'
#' @examples
#' filter_by_country(data, "United States")
#'
#' @export

filter_by_country <- function(data, country) {

  if (!("country" %in% names(data))) {
    stop("Data must contain a 'country' column.")
  }

  filtered <- data[data$country == country, ]

  if (nrow(filtered) == 0) {
    stop("No data found for country = '", country, "'.")
  }

  return(filtered)
}
