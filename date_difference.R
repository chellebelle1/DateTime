date_difference <- function(first_date, second_date, units = "days") {
  
  # eerste datum 
  first <- as.Date(first_date)
  first
  
  # tweede datum
  second <- as.Date(second_date)
  second
  
  # bereken het verschil in dagen 
  date_difference <- second - first
  date_difference
  
  return(date_difference)
}




