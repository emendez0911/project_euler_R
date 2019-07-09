# Problem 36: Double-base palindromes

library(EulerFunctions)


# function to convert to binary number
to_binary <- function(number) {
    binary <- ""
    while (number > 0) {
        b <- number %% 2
        remainder <- floor(number/2)
        number <- remainder
        binary <- paste0(b, binary)
    }
    
    return(binary)
}

palindromes_sum <- 0
for (i in 1:1e6) {
    if (is_palindromic(i)) {
        base2 <- to_binary(i)
        base2 <- number_to_vector(base2)
        reverse <- rev(base2) # base R function to reverse a vector
        
        if (all(base2 == reverse)) {
            palindromes_sum <- palindromes_sum + i
        }
    }
}


# Answer
palindromes_sum