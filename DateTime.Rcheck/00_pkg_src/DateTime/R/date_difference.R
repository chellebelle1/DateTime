#' Calculate the time difference between two dates
#'
#' @param first_date First date in YYYY-MM-DD format.
#' @param second_date Second date in YYYY-MM-DD format.
#' @param units The unit of time difference. Options are "seconds", "minutes", "hours", "days", "weeks", "months", or "years".
#' @return Time difference between two dates in the filled in unit.
#' @export
#'
#' @examples
#' date_difference("2024-05-21", "2024-05-22", units = "days")

date_difference <- function(first_date, second_date, units = "days") {

  # Maak variabelen van de data
  first <- as.Date(first_date)
  second <- as.Date(second_date)

  # Bereken het verschil tussen de ingevulde data in de juiste eenheid
  if (units == "seconds") {
    date_difference_seconds <- difftime(second, first, units = "secs")
    return(date_difference_seconds)
  }

  else if (units == "minutes") {
    date_difference_minutes <- difftime(second, first, units = "mins")
    return(date_difference_minutes)
  }

  else if (units == "hours") {
    date_difference_hours <- difftime(second, first, units = "hours")
    return(date_difference_hours)
  }

  else if (units == "days") {
    date_difference_days <- difftime(second, first, units = "days")
    return(date_difference_days)
  }

  else if (units == "weeks") {
    date_difference_weeks <- difftime(second, first, units = "weeks")
    return(date_difference_weeks)
  }

  else if (units == "months") {
    date_difference_months <- as.numeric(difftime(second, first, units = "days")) / 30.44 # gemiddelde dagen in een maand
    return(paste("Time difference of", date_difference_months, "months"))
  }

  else if (units == "years") {
    date_difference_years <- as.numeric(difftime(second, first, units = "days")) / 365.25 # rekening houden met schrikkeljaren
    return(paste("Time difference of", date_difference_years, "years"))
  }

  else {
    stop("Invalid unit. Please use 'seconds', 'minutes', 'hours', 'days', 'weeks', 'months', or 'years'.")
  }
}

# Examples
date_difference("2024-05-21", "2024-05-22", units = "seconds")
date_difference("2024-05-21", "2024-05-22", units = "minutes")
date_difference("2024-05-21", "2024-05-22", units = "hours")
date_difference("2024-05-21", "2024-05-22", units = "days")
date_difference("2024-05-21", "2024-06-21", units = "weeks")
date_difference("2024-05-21", "2024-09-21", units = "months")
date_difference("2024-05-21", "2025-09-21", units = "years")

