# Problem 18: Maximum path sum I

## 'Copy-paste approach' to get values of the triangle
triangle <- 
"75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"

triangle <- strsplit(triangle, split = "\n")
triangle <- sapply(unlist(triangle), strsplit, split = " ")
triangle <- lapply(triangle, as.numeric)

## We'll try with recursion until reducing the triangle to 2 rows and then determining which is the highest path in each case.

highest_adjacent_n <- function(triangle, temporary_sum = 0) {
    
    for (row in triangle) {
        if (length(row) > 1) { # do after second row 
            
            ## If not the last sub-triangle, then keep sub-dividing.
            if (length(triangle) > 2) {
                triang_a <- list()
                triang_b <- list()
                
                ## Ignoring the first row (already accounted for)
                sub_triang <- triangle[2:length(triangle)] 
                
                for (line in sub_triang) {
                    triang_a[[length(triang_a) + 1]] <- 
                        line[1:(length(line) - 1)]
                    
                    triang_b[[length(triang_b) + 1]] <- 
                        line[2:length(line)]
                }
                
                ta <- highest_adjacent_n(triang_a, temporary_sum)
                tb <- highest_adjacent_n(triang_b, temporary_sum)
                
                temporary_sum <- max(ta, tb) # choosing highest path
                break()
                
            } else {
                ## Choosing highest value in last row of sub-triangle
                temporary_sum <- temporary_sum + max(row) 
                break()
            }
        }
        
        ## Adding first value of sub-path
        temporary_sum <- temporary_sum + row 
    }
    
    return(temporary_sum)
}


# Answer
highest_adjacent_n(triangle)
