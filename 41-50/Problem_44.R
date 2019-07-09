# Problem 44: Pentagon numbers

library(EulerFunctions)


# Generating a list of pentagonal numbers to loop through
pentagonal_numbers <- vector()
for (n in 1:10000) {
    pentagonal_numbers[n] <- n*(3*n - 1)/2
}

len <- length(pentagonal_numbers)
solution <- 0
j <- 0

# Outer for loop moves up and inner for loop down, so we can find the smallest value for Pk - Pj first, for every Pk and Pj 
for (pk in pentagonal_numbers[2:len]) {
    j <- j + 1
    
    for (pj in pentagonal_numbers[j:1]) {
        d1 <- pk + pj
        d2 <- pk - pj
        
        if (is_pentagonal(d1) & is_pentagonal(d2)) {
            solution <- d2
            break()
        }
    }
    
    if (solution) # If solution is found, break outer loop
        break()
}


# Answer
solution
