# Problem 33: Digit cancelling fractions

library(EulerFunctions)


num_prod <- 1
den_prod <- 1

for (num in 11:98) { 
    for (den in (num+1):99) { # value < 1
        if (num %% 10 == 0 & den %% 10 == 0) 
            next() # ignores trivial answers
        
        # dividing numerator and denominator into digits
        num_t <- num %/% 10 
        num_u <- num - (num_t * 10)
        
        den_t <- den %/% 10
        den_u <- den - (den_t * 10)
        
        
        # comparing digits
        fract <- vector()
        if (num_t == den_t)
            fract <- c(num_u,den_u)
        else if (num_t == den_u)
            fract <- c(num_u,den_t)
        else if (num_u == den_t)
            fract <- c(num_t,den_u)
        else if (num_u == den_u)
            fract <- c(num_t,den_t)
        else
            next()
        
        if (fract[1]/fract[2] == num/den) {
            num_prod <- num_prod * num
            den_prod <- den_prod * den
        }
    }
}

gcd <- greatest_common_div(num_prod, den_prod)


# Answer
den_prod/gcd