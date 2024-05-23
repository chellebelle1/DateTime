#' Add a specific amount of time to a given date with time
#'
#' @param datetime The starting datetime in POSIXct format, YYYY-MM-DD HH:MM:SS.
#' @param amount The amount of time to add.
#' @param unit The unit of time to add (seconds, minutes, hours, days, weeks).
#' @return The datetime after adding the specific amount of time.
#' @export
#'
#' @examples
#' add_time("2024-05-21 08:00:00", 1, "hours")  # Add 1 hour

add_time <- function(datetime, amount, unit) {
  # Maak variabele aan van de tijd
  datetime <- as.POSIXct(datetime)

  # Voeg juiste tijd toe met specifieke eenheid
  if (unit == "seconds") {
    new_datetime <- datetime + amount
  } else if (unit == "minutes") {
    new_datetime <- datetime + amount * 60
  } else if (unit == "hours") {
    new_datetime <- datetime + amount * 3600
  } else if (unit == "days") {
    new_datetime <- datetime + amount * 86400
  } else if (unit == "weeks") {
    new_datetime <- datetime + amount * 604800
  } else {
    stop("Invalid unit specified. Please choose from seconds, minutes, hours, days, or weeks.")
  }

  return(new_datetime)
}
