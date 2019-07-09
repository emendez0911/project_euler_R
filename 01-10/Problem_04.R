# Problem 4: Largest palindrome product

library(EulerFunctions)


largest_palindrome <- 0
limit <- 0 

for (i in 999:100) { # Find largest first
    if(i<limit) # If i < limit then also j (i >= j)
        break()
    
    for (j in i:100) {
        # If j < limit, then product will always be less than
        # largest_palindrome for same i
        if(j<limit) 
            break() # terminates inner loop.
        
        product <- i*j
        
        if(is_palindromic(product) & product > largest_palindrome) {
            largest_palindrome <- product
            limit <- j # Assigns new limit 
        }
    }
}

# Answer
largest_palindrome
