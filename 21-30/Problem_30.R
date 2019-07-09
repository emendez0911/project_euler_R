# Problem 30: Digit fifth powers

# The problem doesn't define an upper limit (I know, I thought the same thing). Being 9 the largest, we can consider 9^5 = 59049, which hints that the upper bound should at least have five digits. 
# Further, we can try finding our limit equals to n*(9^5), being n the number of digits.
## For n = 5, 5*(9^5) = 295245 (6 digits)
## For n = 6, 6*(9^5) = 354294 (another 6 digits number)
## For n = 7, 7*(9^5) = 413343 (yet another 6 digits number)...
# We can continue, but 6 seems to be considerable enough.

library(EulerFunctions)


numbers <- vector()
upper_limit <- (9^5)*6


## Skipping one-digit numbers (not that big of a jump anyways).
for (n in 10:upper_limit) {
    # split number into vector of characters
    digits <- number_to_vector(n)
    sum <- 0
    
    for (digit in digits) {
        sum <- sum + digit^5 # find sum of fifts
    } 
    
    if (n == sum) {
        numbers[length(numbers)+1] <- n
    }
}


# Answer
sum(numbers)

