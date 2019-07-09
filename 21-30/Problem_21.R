# Problem 21: Amicable numbers

library(EulerFunctions)

## Find list of amicable pairs
amicable_numbers <- function(top) {
    amicables <- list()
    
    for (a in 2:top) {
        if (a %in% unlist(amicables)) # ignore when already found
            next()
        
        b <- sum(proper_divisors(a))
        if (a == b) # a != b
            next()
        
        if (a == sum(proper_divisors(b)))
            amicables[[length(amicables) + 1]] <- c(a,b)
    }
    
    return(amicables)    
}


## Getting amicable pairs below 10000 
amic_numbers <- amicable_numbers(10000)  


# Answer
sum(unlist(amic_numbers))
