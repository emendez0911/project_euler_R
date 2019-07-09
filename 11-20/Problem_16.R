# Problem 16: Power digit sum

library(EulerFunctions)
library(gmp) # bigInts

number <- pow.bigz(2, 1000) # big int
number <- as.character(number)
digits <- number_to_vector(number)


# Answer
sum(digits)
