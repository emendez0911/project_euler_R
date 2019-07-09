# Problem 32: Pandigital products

# To solve this problem, I did as follows:
## The total number of digits in multiplicand, multiplier and product has to be 9  
## If multiplicand is < 10, then multiplier has to be > 1000
## If multiplicand is > 10, then multiplier has to be > 100

library(EulerFunctions)


pandigital_products <- vector()

for (multiplicand in 2:98) { # excluding 1 and 99-100 for obvious reasons 
    if (multiplicand %% 10 == 0 | multiplicand %% 11 == 0)
        next() # skips multiples of 10 and 11
    
    ## skips numbers where 0 or repeated digits are found
    if (multiplicand < 10) {
        multiplier <- 1234 # > 1000
    } else {
        multiplier <- 123 # > 100
    }
    
    while (multiplicand * multiplier < 10000) {
        if (multiplier %% 10 == 0) { # no multiples of 10
            multiplier <- multiplier + 1
            next()
        }
        
        product <- multiplicand * multiplier
        n <- paste0(multiplicand, multiplier, product)
        pandigital <- is_pandigital(n) #boolean
        
        ## If pandigital and not found yet...
        if (pandigital & !(product %in% pandigital_products)) {
            i <- length(pandigital_products) + 1
            pandigital_products[i] <- product 
        }
        
        multiplier <- multiplier + 1
    }
}


# Answer
sum(pandigital_products)
