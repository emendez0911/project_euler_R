# Problem 46: Goldbach's other conjecture

library(EulerFunctions)


primes <- get_primes(10000)
n <- 9

while (TRUE) {
    n <- n + 2
    
    if (!(n %in% primes)) {
        found <- FALSE
        
        # Loop through primes below to find out whether it can be
        # written as p + 2x^2
        for (p in primes[primes < n]) {
            x <- n - p
            y <- sqrt(x/2) # twice a square
            
            if (y == floor(y)) { # if y is an integer, then TRUE
                found <- TRUE
                break()
            }
        }
        
        if (!found) { # found == FALSE
            break()
        }
    }
}


# Answer
n
