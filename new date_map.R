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

#Question 3: Using map() and map_dbl(), compute the mean, median, and standard deviation for each numeric vector in the following list:
num_lists <- list(c(4, 16, 25, 36, 49), c(2.3, 5.7, 8.1, 11.4), c(10, 20, 30, 40, 50))

#Compute the mean.
mean_values <- map_dbl(num_lists, mean) 
#Compute the median.
median_values <- map_dbl(num_lists, median)
#Compute the sd.
sd_values <- map_dbl(num_lists, sd)
#Create a data frame to store the results.
data_q3 <- data.frame(
  Mean = mean_values,
  Median = median_values,
  Std_Dev = sd_values
)
data_q3

#Question 4: Given a list of mixed date formats, use map() and possibly() from purrr to safely convert them to Date format and extract the month name.
#This ensures that month names like "Aug" are correctly recognized in all systems.
Sys.setlocale("LC_TIME", "C")

date_strings <- list("2023-06-10", "2022/12/25", "15-Aug-2021", "InvalidDate")

#Create a safe function to parse dates while handling errors
safe_parse_date <- possibly(~ as.Date(.x, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%d-%b-%Y")), NA)
#Convert date strings into Date format safely.
converted_dates <- map(date_strings, safe_parse_date)
#Extract the month name from each valid date.
map_chr(converted_dates, ~ if (!is.na(.x)) as.character(month(.x, label = TRUE, locale = "en_US")) else "Invalid")