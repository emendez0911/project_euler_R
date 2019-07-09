# Problem 39: Integer right triangles

library(EulerFunctions)


triples_matrix <- matrix(ncol = 4, nrow = 0)

## Function to add triples
add_triples <- function(triple, p) {
    triple <- triple[order(triple)]
    triples_matrix <<- rbind(triples_matrix, c(triple, p))
}

## Generating pythagorean triples using Euclid's formula
for (m in 2:31) { # for all m > 31, perimeter will be > 1000
    for (n in 1:(m-1)) { # m > n > 0
        
        # avoid duplicates if not coprimes or both odd.
        if (greatest_common_div(m, n) != 1)
            next()
        if (m %% 2 != 0 & n %% 2 != 0)
            next()
            
        perimeter <- 2*m^2 + 2*m*n
        
        if (perimeter <= 1000) {
            a <- m^2 - n^2
            b <- 2*m*n
            c <- m^2 + n^2
            add_triples(c(a,b,c), perimeter)
            
            # Using k-values to generate non-primitive triples
            k <- 2
            while (k * perimeter <= 1000) {
                # storing values in temporal variables
                temp_a <- k*a
                temp_b <- k*b
                temp_c <- k*c
                
                triple <- c(temp_a,temp_b,temp_c)
                p <- sum(triple)
                add_triples(triple, p)
                
                k <- k + 1
            }
        }
    }
}

## 'table' generates a table of frequencies
## Converting to data frame to get a better presentation of the values
df <- as.data.frame(table(triples_matrix[,4]), 
                    stringsAsFactors = F) # no levels/categories in df

## Get value of perimeter with highest freq 
highest <- as.numeric(df[df$Freq == max(df$Freq),1])


# Answer
highest