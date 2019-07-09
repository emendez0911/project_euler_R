# Problem 20: Factorial digit sum

library(EulerFunctions)
library(gmp) # bigInts


factorial <- prod.bigz(100:1)
number <- number_to_vector(factorial)


# Answer
sum(number)
