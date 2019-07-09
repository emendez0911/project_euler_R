# Problem 38: Pandigital multiples

# To solve this problem, I considered the following:
## Since we are looking for pandigital 9-digit numbers, the first largest will result from multiplying 9 with (1:5) == 918273645
## We should only consider numbers starting with 9, so the first product starts with 9 (we already know that the first highest is 918273645). 
## The limit should be < 9999, since every concatenated product of a integer > 9999 with (1,2,..,n) will result in more than 9 digits

library(EulerFunctions)


largest <- 918273645 # first largest == 9 * (1:5)

# Let's avoid 0's and repeated digits in our 'jumps' since the first multiplier is 1 (making null the possibility of obtaining a concatenated pandigital 1-9 number for those cases).   
multiple <- 91 # next jump

while (multiple < 9876) { 
    if (multiple %% 10 == 0) { # skips numbers ending in 0
        multiple <- multiple + 1
        next()
    }
    
    size <- 0
    n <- 1
    concat_prod <- ""
    
    while (size < 9) {
        product <- multiple * n
        concat_prod <- paste0(concat_prod,product)
        n <- n + 1
        size <- nchar(concat_prod)
    }
    
    if (size == 9 & is_pandigital(concat_prod)) {
        concat_prod <- as.numeric(concat_prod)
        
        if (concat_prod > largest) {
            largest <- concat_prod
        }
    }
    
    if (multiple == 98) {
        multiple <- 912 # next jump
        next()
    }
    if (multiple == 987) {
        multiple <- 9123 # next jump
        next()
    }
    
    multiple <- multiple + 1
}


#Answer
largest