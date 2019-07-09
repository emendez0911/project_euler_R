# Problem 37: Truncatable primes

library(EulerFunctions)


# Get vector with the truncated numbers
get_trunc <- function(number) {
    trunc_arr <- vector()
    n_right <- number
    n_left <- number
    
    while (n_right >= 10) {
        # truncate right
        n_right <- floor(n_right/10)
        
        # truncate left
        x <- 10^(nchar(n_left)-1)
        i <- floor(n_left/x)
        n_left <- n_left - i*x
        
        # add to vector
        trunc_arr[length(trunc_arr) + 1] <- n_right
        trunc_arr[length(trunc_arr) + 1] <- n_left
    }
    
    return(trunc_arr)
}


# Adapting regular expression from Problem 35
## The following filters out numbers with: even digits (except those starting with 2), fives (except at the beginning), zeros or starting/ending with 1 or 9 (since 1 and 9 are not primes).
primes <- get_primes(1e6) # generating primes
to_match <- "0|^1|1$|^.+2|4|^.+5|6|8|^9|9$"
filtered_primes <- primes[!grepl(to_match, primes) & 
                              primes > 10]
sum <- 0
count <- 0
for (p in filtered_primes) {
    trunc_nums <- get_trunc(p)
    
    if (all(trunc_nums %in% primes)) {
        count <- count + 1
        sum <- sum + p
    }
    
    # Break loop when the eleventh truncatable prime is found
    if (count == 11) 
        break()
}


# Answer
sum