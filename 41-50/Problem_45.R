# Problem 45: Triangular, pentagonal and hexagonal

library(EulerFunctions)


# Generate next triangular number
n <- 285
next_triangle <- 0

while (TRUE) {
    n <- n + 1
    t <- n*(n+1)/2
    
    if (is_pentagonal(t) & is_hexagonal(t)) {
        next_triangle <- t
        break()
    }
}


# Answer
next_triangle