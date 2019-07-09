# Problem 49: Prime permutations

library(EulerFunctions)


primes <- get_primes(10000)
primes <- primes[primes > 1487] # get primes > 1487

numbers <- vector()
for (p in primes) {
    # My approach to this problem is to generate the permutations
    # and then remove those that are not primes
    p_vector <- number_to_vector(p)
    perms <- permute(p_vector)
    perms <- perms[perms %in% primes]
    perms <- perms[order(perms)] # order ascending
    len <- length(perms)
    
    if (len < 3)
        next()
    
    # After getting a vector of prime permutations, the rest is 
    # just finding the three numbers looping through that vector
    i <- 1
    for (x in perms) {
        i <- i + 1
        for (y in perms[i:len]) {
            k <- y - x
            z <- y + k
            
            if (z >= 10000)
                break()
            
            if (z %in% perms) {
                numbers <- c(x, y, z)
                break()
            }
        }
        
        if (length(perms[i:len]) < 3)
            break() 
    }
    
    if (length(numbers) > 0) 
        break() # break when finding the numbers
}


# Answer
paste0(numbers, collapse = '')
