# Problem 12: Highly divisible triangular number

# Counting the factors of a number
count_factors <- function(number) {
    factors <- 0
    
    if (number == 1)
        return(1)
    
    for (i in 1:sqrt(number)) {
        if (number %% i == 0) {
            factors <- factors + 2
        } 
           
        if (i * i == number)
            factors <- factors - 1 # minus one when perfect square
    }
    
    return(factors)
}


## According to number theory, if n is the mth triangular number then:
## n = m*(m+1)/2

triang_number <- function(n) {
    m <- 1
    triangular_number <- m*(m + 1)/2
    
    ## Since m and m + 1 are coprimes (gcd = 1), we know that for two coprime numbers, a and b, the number of factors of 'a' multiplied by the number of factors of 'b' equals the number of factors of 'a * b'
    
    ## In each iteration, either m or m + 1 is going to be even. So dividing one of them by 2 is a way to consider (get rid of) the divisor in formula n = m*(m + 1)/2
    
    a <- count_factors(m)
    b <- count_factors((m+1)/2) 
    
    while (a * b <= n) { 
        m <- m + 1
        
        if (m %% 2 == 0) { 
            a <- count_factors(m/2)
            b <- count_factors(m+1)
        } else {
            a <- count_factors(m)
            b <- count_factors((m+1)/2)
        }
        
        triangular_number <- m*(m + 1)/2
    }
    
    return(triangular_number)
}


# Answer
triang_number(500)
