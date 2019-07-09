# Problem 15: Lattice paths

## Using Pascal's triangle logic to find the route to each vertice based on the binomial coefficients.

find_routes <- function(rows, cols) {
    # +1 since every number in the grid will represent a vertice
    rows <- rows + 1 
    cols <- cols + 1 
    grid <- matrix(nrow = rows, ncol = cols) 
    routes <- 0
    
    for (i in 1:rows) {
        for (j in 1:cols) {
            # First row and first column equal to 1 
            # since there is only one way to access that vertice.
            if (i == 1 | j == 1)
                grid[i,j] <- 1
            else 
                grid[i,j] <- grid[i,j-1] + grid[i-1,j]
        }
    }
    
    routes <- grid[rows,cols]

    return(routes)
}


# Answer
find_routes(20,20)


