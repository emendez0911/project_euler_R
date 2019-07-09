# Consecutive prime sum

library(EulerFunctions)


primes <- get_primes(1000000)

# Initially set values for numbers below 1000
consec_primes <- 21 
longest_sum <- 953
i <- 1
len <- length(primes)

while (TRUE) {
    sum <- 0
    count <- 0
    
    for (p in primes[i:len]) {
        sum <- sum + p
        count <- count + 1
        
        if (sum > 1000000)
            break()
        
        if (sum %in% primes) {
            if (count > consec_primes) {
                consec_primes <- count
                longest_sum <- sum
            }
        }
    }
    
    # The following checks if necessary to continue the loop.
    # If sum of next n primes, for n == highest number of 
    # consecutive primes yet, is greater than upper limit (1e6)
    # then we've found the largest.
    i <- i + 1
    if (sum(primes[i:(i+consec_primes)]) > 1000000)
        break()
}


# Answer
longest_sum
