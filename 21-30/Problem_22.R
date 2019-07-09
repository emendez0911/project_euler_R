# Problem 22

file <- "https://projecteuler.net/project/resources/p022_names.txt"

## There is a name == "NA", 
## therefore changing default value to "" for na.strings 
names <- scan(file, what = "", sep = ",", na.strings = "") 
names <- names[order(names)] # ordered vector

total_score <- 0
position <- 0

for (name in names) {
    alph_value <- 0
    position <- position + 1
    spelled_name <- unlist(strsplit(name, split = "")) # letters in name
    
    for (char in spelled_name) {
        value <- which(LETTERS == char) # LETTERS is a vector of letters
        alph_value <- alph_value + value
    }
    
    score <- alph_value * position
    total_score <- total_score + score
}


# Answer
total_score