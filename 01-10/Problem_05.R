# Problem 5: Smallest multiple

library(EulerFunctions)


# Using base R 'lapply' to vectorize the function.
# Just considering 11-20 since these numbers are already multiples of 1-10
factors_list <- lapply(11:20, prime_factors)

# Saving values into a data frame
df <- data.frame()

for (factors in factors_list) {
    # 'table' creates a table of frequencies (times each element repeated)
    # 'rbind' combines the results
    df <- rbind(df, as.data.frame(table(factors),
                                  stringsAsFactors = F))
}

colnames(df) <- c("p_factor", "exponent")

# Obtainig vector of unique elements 
unique_factors <- as.numeric(unique(df$p_factor))


# Calculating least common multiple (lcm), which is the product of the factors, each elevated to the maximum exponent they appear 
lcm <- 1

for (factor in unique_factors) {
    e <- max(df[df$p_factor == factor, "exponent"]) # max exponent
    lcm <- lcm*(factor^e)
}


# Answer
lcm 
