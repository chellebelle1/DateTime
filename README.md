# DateTime
R package for calculations with time and dates. 

## Installation

To install the `DateTime` package from GitHub, use the following command in R:
```R
devtools::install_github("your_username/DateTime")
```

### The package contains 4 functions which are related to date and time.

- `date_difference()`: allows you to calculate the difference between two dates. The function uses various time units such as seconds, minutes, hours, days, weeks, months, and years.

- `add_time()`: adds a certain amount of time to the date of your choice.

- `is_same_day()`: checks whether two dates fall on the same weekday. If so, it returns “TRUE” and gives the name of the weekday. If not, it returns “FALSE” and gives you the two different weekday names.

- `is_dutch_holiday()`: checks if the given date, is also a (legal) Dutch holiday. These holidays are: New Year's Day, Good Friday, Easter, Second Easter Day, King's Day, Liberation Day (every 5 years), Ascension Day, Pentecost, Second Pentecost, Christmas and Second Christmas Day.

#### Licence
This package is licensed under the MIT License. See the `LICENCE` file for details.
