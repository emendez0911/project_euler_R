# Problem 19: Counting Sundays

## We'll use a POSIXlt object, which contains details of the dates including the weekday (wday) and day of the month (mday).

start <- as.Date("01/01/1901", format = "%d/%m/%Y") 
end <- as.Date("31/12/2000", format = "%d/%m/%Y")

## Creating a sequence of dates between 01/01/1901 and 31/12/2000
date_range <- as.POSIXlt(seq(start, end, "days")) 

# 0 == 'Sunday'
sundays <- sum(date_range$wday == 0 & date_range$mday == 1)


# Answer
sundays
