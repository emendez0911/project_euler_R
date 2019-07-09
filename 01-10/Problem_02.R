# Problem 2: Even Fibonacci numbers

fib_seq <- c(1,2)
sum_even <- 2 # initialize with 2 (first even number in the seq)
n <- 4000000 

while (TRUE) {
    idx <- length(fib_seq) # Index of last element
    n_term <- fib_seq[idx] + fib_seq[idx-1] # new term in Fibonacci seq
    
    if (n_term <= n) { # ...whose values do not exceed "n"
        fib_seq[idx+1] <- n_term
        
        if (n_term%%2 == 0) { # Is even?
            sum_even <- sum_even + n_term
        }
    }
    else {
        break() # end loop
    }
}

# Answer
sum_even
