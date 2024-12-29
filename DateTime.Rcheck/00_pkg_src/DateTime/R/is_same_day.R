#' Check if two dates fall on the same day
#'
#' @param date1 The first date in YYYY-MM-DD format.
#' @param date2 The second date in YYYY-MM-DD format.
#' @return A list with the logical value telling whether the dates fall on the same day, and if not, the days of the week on which they fall.
#' @export
#'
#' @examples
#' is_same_day("2024-05-21", "2024-05-21")
#' is_same_day("2024-05-04", "2024-02-02")

# maak variabele aan van de data
is_same_day <- function(date1, date2) {
  date1 <- as.Date(date1)
  date2 <- as.Date(date2)

  # check of ze vallen op dezelfde dag
  same_day <- date1 == date2
  if (same_day) {
    day_of_week <- weekdays(date1)
    return(list(same_day = same_day, day_of_week = day_of_week))
  } else {
    day_of_week_1 <- weekdays(date1)
    day_of_week_2 <- weekdays(date2)
    return(list(same_day = same_day, day_of_week_1 = day_of_week_1, day_of_week_2 = day_of_week_2))
  }
}
