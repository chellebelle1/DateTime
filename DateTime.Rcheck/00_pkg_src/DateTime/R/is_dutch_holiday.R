#' Check if a date is a holiday in the Netherlands
#'
#' @param date The date to check in YYYY-MM-DD format.
#' @return TRUE if the date is a holiday, FALSE otherwise.
#' @export
#'
#' @examples
#' is_dutch_holiday("2024-01-01")
#' is_dutch_holiday("2024-04-27")
#'
is_dutch_holiday <- function(date) {
  # Alle (wettelijke) Nederlandse feestdagen
  dutch_holidays <- c(
    "01-01",    # New Year's Day
    "03-29",    # Goede Vrijdag / Good Friday
    "03-31",    # Easter
    "04-01",    # Second Easter Day
    "04-27",    # King's Day
    "05-05",    # Liberation Day (every 5 years)
    "05-09",    # Ascension Day
    "05-19",    # Pentecost
    "05-20",    # Second Pentecost
    "12-25",    # Christmas Day
    "12-26"     # Second Christmas Day
  )

  # Check if the date is a Dutch holiday
  is_holiday <- format(as.Date(date), "%m-%d") %in% dutch_holidays

  return(is_holiday)
}
