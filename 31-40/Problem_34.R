# Problem 34: Digit factorials

library(EulerFunctions)


# Function to obtain the sum of the factorial of the digits
digit_factorials_sum <- function(digits) {
    # Pre-calculated factorials of numbers from 1-9
    factorials <- c(1, 2, 6, 24, 120, 720, 5040, 40320, 362880)
    
    result <- 0
    
    for (digit in digits) {
        if (digit == 0) # 0! == 1
            result <- result + 1
        else
            result <- result + factorials[digit]
    }
    
    return(result)
}

# Using same logic as in Problem 30 (because there's no upper limit given), since 9 is the largest possible digit then:
## 9! == 362880 (6 digits)
## 6 * 9! == 2177280 (7 digits)
## 7 * 9! == 2540160 (7 digits)
## 8 * 9! == 2903040 (7 digits) <- which means that no 8-digits number is equal to the sum of the factorials of its digits. Therefore, 7 digits is our upper limit

numbers <- vector()
range <- 10:2540160 # skips one-digit numbers

for (i in range) {
    digits <- number_to_vector(i)
    fact_sum <- digit_factorials_sum(digits)
    
    if (fact_sum == i)
        numbers[length(numbers) + 1] <- i
}


# Answer
sum(numbers)

# PS: Still a slow algorithm (around 2 mins or so). I tried adding extra validation but it either didn't make a difference or made the process even slower. 