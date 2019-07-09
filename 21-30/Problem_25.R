# Problem 25: 1000-digit Fionacci number

library(gmp) # bigInts

# Function that gets the number of digits as parameter and returns the index.
fib <- function(n) {
    if (n == 1)
        return(1) # since F(1) = 1
    
    a <- 1 # F(1)
    b <- 1 # F(2)
    k <- 2 # index (considering a, b firts two numbers)
    
    while (TRUE) {
        k <- k + 1
        temp <- as.bigz(a)
        a <- as.bigz(b)
        b <- add.bigz(temp, b)
        
        if(nchar(as.character(b)) == n)
            return(k)
    } 
}


# Answer
fib(1000)
