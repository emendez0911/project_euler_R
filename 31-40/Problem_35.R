# Problem 35: Circular primes

library(EulerFunctions)


# function to return number rotations
rotate <- function(n_vector) {
    rotations <- vector()
    n <- length(n_vector)
    
    if (n == 1)
        return(n_vector)
    
    for (i in 1:n) {
        rotations[i] <- vector_to_number(n_vector)
        n_vector[n+1] <- n_vector[1]
        n_vector <- n_vector[-1]
    }
    
    return(rotations)
}

## Solution implementation
primes <- get_primes(1e6)

# Let's filter out those with even numbers, zero or five, since one of the rotations will surely be a composite number
to_match <- "0|2|4|6|8|5" # pattern to match
primes <- primes[!grepl(to_match, primes) | primes < 10]

circ_primes <- vector()

for (p in primes) {
    if (p < 10) {
        circ_primes[length(circ_primes)+1] <- p
        next()
    }
    
    # Convert to vector to get rotations 
    p <- number_to_vector(p)
    rotations <- rotate(p)
    
    # If all rotations are prime numbers, then add to circular primes.
    if (all(rotations %in% primes)) {
        circ_primes[length(circ_primes)+1] <- vector_to_number(p)
    }
}


# Answer
length(circ_primes)