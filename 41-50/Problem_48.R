# Problem 48: Self powers

library(gmp) # bigInts

number <- 0
for (n in 1:1000) {
    self_pow <- pow.bigz(n,n)
    number <- add.bigz(number, self_pow)
}


# Answer
mod.bigz(number, 10^10) # applying modulus to get last digits
