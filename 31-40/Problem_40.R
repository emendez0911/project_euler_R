# Problem 40: Champernowne's constant


## Solution 1: Lazy approach

## We can create a string 'number' with the concatenated numbers from 1:n, with n so that length(number) >= largest index we're looking for (1e+06)

number <- paste0(1:200000, collapse = '') # object size == 1.089 MB
indexes <- 10^(0:6)
result <- 1

for (i in indexes) {
    dn <- as.numeric(substr(number,i,i)) # digit at index 
    result <- result * dn 
}

## Answer no.1
result


## Solution 2: Functional approach

## We'll try first finding the complete number at the specified position in the decimal part and then find the digit of that number where we are standing.
## This is fairly easy to do, but I'll try to explain the code below with a couple of extra variables...

get_decimal_digit <- function(position) {
    if (position < 10) # return same number for 1-9
        return(position)
    
    # Logic: 
    ## From 1-9 --> 9 numbers
    ## From 10:99 --> 90 * 2-digits numbers (180)
    ## From 100:999 --> 900 * 3-digits numbers (2700)
    ## And so on...
    
    j <- position
    x <- 9
    e <- 0
    trace <- 0 # set trace to later identify number
    digits <- 1 # number of digits at position
    
    # Let's find our number j, that identifies the numbers
    # after a change of digits to the position in question
    # (i.e. after 9, 99, 999...) 
    
    while (j > x) {
        trace <- trace + (9 * 10^e) # 9, 99, 999...
        j <- j - x
        e <- e + 1 # increment exponent to get 90, 900, 900...
        digits <- digits + 1 # digit change
        
        # increment x exponentially as defined in logic
        x <- (9 * 10^e) * digits
    }
    
    k <- j/digits # complete numbers after change of digits
    number_at_position <- trace + ceiling(k) # round up k with ceiling
    
    # Now let's find our specific digit.
    # Each digit will be represented as a 'fraction' of the complete 
    # number at position.
    dn <- 0
    fraction <- round(k - floor(k), 4)  
    i <- round(1/digits,4) 
    
    if (fraction == 0) { # last digit of number at position
        dn <- number_at_position %% 10
    } else { # truncate number from left until reaching the fraction
        left_trunc <- number_at_position
        
        while (i <= fraction) {
            dn <- left_trunc %/% 10^e
            left_trunc <- left_trunc - dn*10^e
            e <- e - 1
            i <- i + round(1/digits,4) 
        }    
    }
    
    return(dn)
}


## Answer no.2
indexes <- 10^(0:6)
prod(sapply(indexes, get_decimal_digit))
