# Problem 41: Pandigital prime

## I'll use permutations to generate the n-digits pandigital numbers, then verify if it's prime with a primality test

library(EulerFunctions)


digits <- 9:1
largest_pp <- 0

while (largest_pp == 0) { # repeat until finding largest
    # skip numbers divisible by 3
    if (sum(digits) %% 3 == 0) {
        digits <- digits[-1]
        next()
    }
    
    perms <- permute(digits)
    perms <- perms[!(perms %% 2 == 0)] # skip even numbers
    perms <- perms[order(perms, decreasing = T)]
    
    for (p in perms) {
        if (is_prime(p)) {
            largest_pp <- p # first found is largest
            break()
        }
    }
}


# Answer
largest_pp