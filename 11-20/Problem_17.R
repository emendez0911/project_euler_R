# Problem 17: Number letter counts

## The following is probably the very definition of overcomplicating something quite simple. It can definitely be improved (e.g. avoid re-counting after 100), but I will leave it as-is for now.

number_letter_count <- function(top) {
    numbers <- c("one", "two", "three", "four", "five",
                 "six", "seven", "eight", "nine", "ten",
                 "eleven", "twelve", "thirteen", "fourteen", "fifteen",
                 "sixteen", "seventeen", "eighteen", "nineteen",
                 "twenty", "thirty", "forty", "fifty", "sixty", 
                 "seventy", "eighty", "ninety")
    
    units <- nchar(numbers[1:20]) # 1 - 20
    tens <- nchar(numbers[c(10,20:27)]) # 10, 20, ..., 90
    hundred <- 7 # letters
    thousand <- 8 # letters
    letters_used <- 0
    u <- 0 # units
    t <- 0 # tens
    h <- 0 # hundreds
    th <- 0 # thousands
    
    for (i in 1:top) {
        count <- 0
        
        u <- u + 1 # increasing units
        
        if (u == 10) { # increasing  tens
            t <- t + 1
            u <- 0
        }
        if (t == 10) { # increasing hundreds
            h <- h + 1
            t <- 0
        }
        if (h == 10) { # increasing thousands
            th <- th + 1
            h <- 0
        }
        
        
        ## Handle first twenty numbers individually
        if (i %% 100 > 0 & i %% 100 <= 20 ) { 
            if (i > 1000) 
                letters_used <- letters_used + units[th] + thousand
            
            ## only consider when there are hundreds (101, 1101...)
            if (i %% 1000 > 100 )
                letters_used <- letters_used + units[h] + hundred
            
            if (i > 100)
                letters_used <- letters_used + 3 # 'and'
                
            letters_used <- letters_used + units[i %% 100] 
            next()
        } 
        
        if (th > 0) {
            count <- units[th] + thousand
            
            if (h == 0 & t >= 2) {
                count <- count + 3 # 'and'
            }
        }
        if (h > 0) {
            count <- count + units[h] + hundred
            
            if (t >= 2) {
                count <- count + 3 # 'and'
            }
        }
        if (t >= 2) {
            count <- count + tens[t]     
        }
        if (u != 0) {
            count <- count + units[u]
        } 
            
        
        letters_used <- letters_used + count
    }
    
    return(letters_used)
}


# Answer
number_letter_count(1000)
