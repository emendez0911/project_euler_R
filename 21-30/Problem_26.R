# Problem 26: Reciprocal cycles

# According to some properties of periodic decimals, the period of 1/k for integer k is always ≤ k − 1. Primes with primitive root 10 are those for which the decimal expansion of 1/p has period p-1 (cyclic numbers), which is the greatest period possible for any integer (see https://oeis.org/A001913).
# I'll use a combination of primitive root and long division to assess only those greater than the largest cyclic number < 1000.

library(EulerFunctions)
library(gmp) # bigInts


# Determine if number is cyclic
is_cyclic_number <- function(p) {
    if (p %in% c(2,5)) # cannot be 2 or 5
        return(FALSE)
    
    results <- vector() 
    for (k in 0:(p - 2)) {
        i <- as.numeric(pow.bigz(10, k) %% p)
        
        if (i %in% results) {
            return(FALSE)
        }
        results[k+1] <- i
    }
    
    return(TRUE)
}

# Find the longest recurring cycle >= largest_cyclic - 1 
longest_cyclic_num <- function(largest_cyclic_prime, top) {
    cyc_num <- largest_cyclic_prime # longest cyclic number yet
    longest_cyc <- largest_cyclic_prime - 1 # longest cycle yet
    from <- cyc_num + 1 # next number to try
    
    for (num in from:top) {
        x <- 1
        remainders <- vector()
        
        while (TRUE) { # applying long division
            x <- x * 10
            
            if (x %% num == 0) {
                break() # not a periodic decimal number
            } else {
                rem <- x %% num
                
                ## repeated remainder will restart cycle
                if (rem %in% remainders) { 
                    start <- min(which(remainders == rem))
                    end <- length(remainders)
                    cycle_length <- length(remainders[start:end])
                    
                    if (cycle_length > longest_cyc)
                        cyc_num <- num
                    
                    break() # breaks while loop
                } else {
                    remainders[length(remainders) + 1] <- rem
                    x <- rem
                }
            }
        }
    }
    
    return(cyc_num)
}

primes <- get_primes(999)
primes <- primes[order(primes, decreasing = TRUE)]

largest_cyclic_prime <- 0
for (i in primes) {
    if (is_cyclic_number(i)) {
        largest_cyclic_prime <- i
        break()
    }
}


# Answer
longest_cyclic_num(largest_cyclic_prime, 999)
