# Problem 28: Number spiral diagonals

size <- 1001 * 1001
diagonal <- vector()
diagonal[1] <- 1
n <- 1
increment <- 2 # the spiral grows in size by two
count <- 0

while(n < size) {
    n <- n + increment
    diagonal[length(diagonal) + 1] <- n
    count <- count + 1
    
    if (count == 4) { # four edges
        increment <- increment + 2 # increment size
        count <- 0 # reset count
    }
}


# Answer
sum(diagonal)
