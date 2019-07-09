# Problem 43: Sub-string divisibility

library(EulerFunctions)


# Get pandigital numbers with divisible sub-strings
substr_divisible <- function(number, p) {
    rest <- digits[!(digits %in% number)] # unused digits
    sub_total <- 0
    
    if (p != 7) { # if last substring not reached
        concat_num <- vector()
        i <- length(number)
        
        for (j in rest) {
            # next tripple == last two digits from current
            # number + next available digit 
            n_triple <- vector_to_number(c(number[(i-1):i],j))
            
            if (n_triple %% primes[p] == 0) {
                p <- p + 1
                concat_num <- c(number, j)
                sbstr <- substr_divisible(concat_num, p)
                sub_total <- sub_total + sbstr
                
                # reset p to previous value, so the loop 
                # continues with same value for p
                p <- p - 1
            }
        }
    } else {
        # When reached last sub-string, add remaining digit
        # as first digit of the number
        concat_num <- paste0(rest, vector_to_number(number))
        concat_num <- as.numeric(concat_num)
        return(concat_num)
    }
    
    return(sub_total)
}

# Get first substr divisible by 2
first_triple <- 12:987
first_triple <- first_triple[first_triple %% 2 == 0] # even

for (number in first_triple) {
    i <- which(first_triple == number) # index
    
    # We are padding with zeros the numbers below 100 to form
    # the triple, so no extra zeros are allowed
    if (number < 100 & number %% 10 == 0) {
        first_triple <- first_triple[-i] # removes element
        next()
    }
    
    # Eliminate numbers with repeated digits
    n <- unique(number_to_vector(number))
    if (length(n) != nchar(number))
        first_triple <- first_triple[-i]
        
}

# Get final sum looping through firt possible sub-strings already identified in vector "first_tripple"
primes <- c(3,5,7,11,13,17)
digits <- 0:9
pandigital_sum <- 0

for (number in first_triple) {
    number <- number_to_vector(number)
    
    if (length(number) < 3)
        number <- c(0, number) # add leading 0 to 2-digit numbers
    
    n <- substr_divisible(number,1)
    pandigital_sum <- pandigital_sum + n
}


# Answer
pandigital_sum
