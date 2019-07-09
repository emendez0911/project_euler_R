# Problem 47: Distinct prime factors

library(EulerFunctions)


primes <- get_primes(500000)
numbers <- vector()
n <- 1

while (TRUE) { # It's simple but it's gonna take its time.
    n <- n + 1
    
    if (n %in% primes)
        next()
    
    factors1 <- unique(prime_factors(n, primes))
    
    if (length(factors1) == 4) {
        factors2 <- unique(prime_factors(n+1, primes))
        
        if (length(factors2) == 4) {
            factors3 <- unique(prime_factors(n+2, primes))
            
            if (length(factors3) == 4) {
                factors4 <- unique(prime_factors(n+3, primes))
                
                if (length(factors4) == 4) {
                    numbers <- c(n:(n+3))
                    break()
                } else {
                    n <- n + 3
                }
                
            } else {
                n <- n + 2    
            }
            
        } else {
            n <- n + 1    
        }
    }
}


# Answer
numbers
