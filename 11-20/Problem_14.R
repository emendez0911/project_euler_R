# Problem 14: Longest Collatz sequence

longest_chain <- function(top_limit) {
    longest <- 0
    start <- 0
    chain_lens <- vector()
    
    for (i in 1:top_limit) {
        n <- i # initial number
        len <- 1 # initial length
        
        while (n != 1) {
            if (n <= i-1) { # added to cache previously found values 
                len <- len + chain_lens[n]
                break()
            }
            
            if (n %% 2 == 0) # if is even
                n <- n/2
            else             # if is odd
                n <- 3*n + 1
            
            len <- len + 1 
        }
        
        if (len > longest) { # is it the longest chain yet?
            longest <- len
            start <- i
        }
        
        chain_lens[i] <- len # add to cache'd values
    }
    
    return(start)
}


# Answer
longest_chain(1000000)