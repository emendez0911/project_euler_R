# Problem 7: 10001st prime

library(EulerFunctions)


## Find the next prime through trial division.
nth_prime <- 0
i <- 2 # skip first two primes (2,3)
next_n <- 5 # next value to test if prime (last found prime + 2)

while (TRUE) {
    if (is_prime(next_n)) { # 'is_prime' uses trial division
        i <- i + 1
        
        if (i == 10001) {
            nth_prime <- next_n
            break()
        }
    }
    
    next_n <- next_n + 2     
}


# Answer
nth_prime

