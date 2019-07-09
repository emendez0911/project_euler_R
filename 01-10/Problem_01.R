# Problem 1: Multiples of 3 and 5

total <- 0
n <- 1000

for (i in 1:(n-1)) {
    if(i%%3 == 0 | i%%5 == 0)
        total <- total + i
}

# Answer
total