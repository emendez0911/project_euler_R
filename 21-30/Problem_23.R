# Problem 23: Non-abundant sums

library(EulerFunctions)

# Determine if a number is abundant
is_abundant <- function(number) {
    if (sum(proper_divisors(number)) > number)
        TRUE
    else
        FALSE
}

# Find abundant numbers below 28123
abundant_numbers <- which(sapply(1:28123, is_abundant))

# Find numbers resulting from the sum of two abundant numbers 
abundant_sums <- vector()
len <- length(abundant_numbers)
i <- 0

for (n in abundant_numbers) {
    i <- i + 1
    
    if (n + n > 28123) # break when reached numbers > 28123/2 
        break()
    
    for (m in abundant_numbers[i:len]) {
        if (n + m > 28123) # if greater than top limit, break
            break()
        
        abundant_sums[length(abundant_sums) + 1] <- n + m
    }
}

# Keep unique values
abundant_sums <- unique(abundant_sums)

# Sum of numbers which cannot be written as the sum of two abundant numbers (not in 'abundant_sums')
non_abundant_sum <- sum(which(!(1: 28123 %in% abundant_sums)))


# Answer
non_abundant_sum