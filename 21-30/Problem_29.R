# Problem 29: Distinct powers

## Solution 1: filtering unique values
numbers <- vector()

for (a in 2:100) {
    for (b in 2:100) {
        num <- a^b
        
        if (num %in% numbers) {
            next()
        }
        
        numbers[length(numbers)+1] <- num
    }
}


# Answer
length(numbers)


## Solution 2: using base R function "unique" after
numbers <- vector()

for (a in 2:100) {
    for (b in 2:100) {
        num <- a^b
        numbers[length(numbers)+1] <- num
    }
}

# Answer
length(unique(numbers))
