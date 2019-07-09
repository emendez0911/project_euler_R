# Problem 9: Special Pythagorean triplet

# Since for every a, b, c, there is a pair m, n, such that:
# a <- m^2 - n^2
# b <- 2mn
# c <- m^2 + n^2 

# Knowing that a + b + c = 1000, then
# m^2 - n^2 + 2mn + m^2 + n^2 = 1000
# which is reduced to:
# 2m^2 + 2mn = 1000 

# Resolving n:
# n = 500/m - m
# Knowing that m and n are both natural integers, and m > n,
# we can do as follows:

triplet <- vector()

for (m in 1:500) { # loop through numbers from 1 to 500
    n <- 500/m - m
    
    # identify positive integers, for which m > n
    if (500 %% m == 0 & m > n & n > 0) { 
        a <- m^2 - n^2 
        b <- 2 * m * n
        c <- m^2 + n^2
        
        if (a + b + c == 1000) { # do we have our triplet?
            triplet <- c(a, b, c)
        }
    }
}


# Answer
prod(triplet)