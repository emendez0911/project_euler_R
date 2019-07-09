# Problem 42: Coded triangle numbers

library(EulerFunctions)


file <- "https://projecteuler.net/project/resources/p042_words.txt"
words <- scan(file, what = "", sep = ",")
triangle_words <- 0

for (word in words) {
    word <- unlist(strsplit(word, split = ""))
    word_value <- 0
    
    for (l in word) {
        # r has already a vector with the letters
        letter_value <- which(LETTERS == l)
        word_value <- word_value + letter_value
    }
    
    if (is_triangular(word_value))
        triangle_words <- triangle_words + 1
}


# Answer
triangle_words
