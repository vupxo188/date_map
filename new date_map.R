library(lubridate)
library(purrr)

#Question 1: Generate a sequence of dates from January 1, 2015 to December 31, 2025, spaced by every two months. Extract the year, quarter, and ISO week number for each date.
date_seq <- seq(ymd("2015-01-01"), ymd("2025-12-31"), by = "2 months")

date_ext <- data.frame(
  date = date_seq,
  year = map_dbl(date_seq, year), # Extract the year from each date      
  quarter = map_dbl(date_seq, quarter), # Extract the quarter from each date
  iso_week = map_dbl(date_seq, isoweek) # Extract the ISO week number from each date
)

date_ext

#Question 2: Given the following dates, compute the difference in months and weeks between each consecutive pair.
sample_dates <- as.Date(c("2018-03-15", "2020-07-20", "2023-01-10", "2025-09-05"))

#Calculate the difference in months between each consecutive pair of dates.
month_diff <- map2_dbl(sample_dates[-length(sample_dates)], #Take all elements except the last one.
                       sample_dates[-1], #Take all elements except the first one.
                       ~ as.period(interval(.x, .y))$month + as.period(interval(.x, .y))$year * 12) #years converted to months + remaining months.
week_diff <- map2_dbl(sample_dates[-length(sample_dates)],  
                      sample_dates[-1], 
                      ~ as.numeric(difftime(.y, .x, units = "weeks"))) #Compute the total week difference.
#Create a data frame to display the results.
data.frame(
  Start_Date = sample_dates[-length(sample_dates)],
  End_Date = sample_dates[-1],
  Month_Difference = (month_diff),
  Week_Difference = (week_diff)    
)
