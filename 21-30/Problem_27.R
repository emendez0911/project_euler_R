# Problem 27: Quadratic primes

## For n = 0, f(n) = n^2 + an + b = b, therefore b has to be a prime >= 2
## We can generate all primes with a sieve for a list of possible b's.

library(EulerFunctions)


# function to calculate number of consecutive primes
quadratic_formula_result <- function(a,b) {
    n <- 0
    
    while (TRUE) { 
        result <- n^2 + a*n + b
        
        if (result < 1000) { # try first with sieve of primes < 1000
            if (result %in% b_coeffs) {
                n <- n + 1
            } else {
                break()
            }
        } else if (is_prime(result)) { # try primality test otherwise
            n <- n + 1
        } else {
            break()
        }
    }
    
    return(n)
}

b_coeffs <- get_primes(1000)
max_nprimes <- 0
coeffs <- vector()

for (a in -999:999) {
    for (b in b_coeffs) {
        # Since for n = 1, f(n) = n^2 + an + b = 1 + a + b
        # The result of the previous has to be odd (or 2) 
        # for there to possibly be more than one prime
        result <- 1 + a + b
        if (result %% 2 == 0 & result > 2) {
            break()
        }
        
        result <- quadratic_formula_result(a,b)
        
        if (result > max_nprimes) {
            max_nprimes <- result
            coeffs <- c(a,b) 
        }
    }
}


# Answer
prod(coeffs)
