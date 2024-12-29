## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, eval=TRUE, echo=TRUE----------------------------------------------
# install packages "usethis" and "devtools" if they are not already installed 
library(knitr)
library(rmarkdown)
library(usethis)
library(devtools)

# Load the DateTime package in 
library(DateTime)

## ----date_difference, echo=TRUE, eval=TRUE------------------------------------
# Load the DateTime package
library(DateTime)

# Calculate the difference between two dates in days
date_difference("2024-05-21", "2024-05-22", units = "days")
# Calculate the difference between two dates in weeks
date_difference("2024-05-21", "2024-06-21", units = "weeks")
# Calculate the difference between two dates in years
date_difference("2024-05-21", "2025-05-22", units = "years")


## ----add_time, eval=TRUE, echo=TRUE-------------------------------------------
# Add 1 hour
add_time("2024-05-21 08:00:00", 1, "hours") 
# Add 2 weeks 
add_time("2024-07-22 07:00:00", 2, "weeks") 
# Add 54 seconds
add_time("2024-05-21 08:00:00", 54, "seconds") 

## ----same day, eval=TRUE, echo=TRUE-------------------------------------------
# Check if the 21st of May 2024 falls on the same day as 21st of May 2024. 
is_same_day("2024-05-21", "2024-05-21")
# Check if the 4th of May 2024 falls on the same day as the 2nd of February 2024
is_same_day("2024-05-04", "2024-02-02")

## ----Dutch holiday, eval=TRUE, echo=TRUE--------------------------------------
is_dutch_holiday("2024-01-01")
is_dutch_holiday("2024-08-12")

