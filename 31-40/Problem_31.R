# Problem 31: Coin sums

## Lazy approach: Nested loops (It obviously takes ages, so DO NOT try this at home). I'll leave it here commented anyways.

# n_combinations <- 0
# 
# for (a in seq(0,200)) { # 1p
#     for (b in seq(0,200,2)) { # 2p
#         if (a+b > 200)
#             break()
#         for (c in seq(0,200,5)) { # 5p
#             if (a+b+c > 200)
#                 break()
#             for (d in seq(0,200,10)) { # 10p
#                 if (a+b+c+d > 200)
#                     break()
#                 for (e in seq(0,200,20)) { # 20p
#                     if (a+b+c+d+e > 200)
#                         break()
#                     for (f in seq(0,200,50)) { # 50p
#                         if (a+b+c+d+e+f > 200)
#                             break()
#                         for (g in seq(0,200,100)) { # 1 pound
#                             if (a+b+c+d+e+f+g > 200)
#                                 break()
#                             
#                             if (a+b+c+d+e+f+g == 200)
#                                 n_combinations <- n_combinations + 1
#                         }
#                     }
#                 }
#             }
#         }
#     }
# }
# 
# n_combinations <- n_combinations + 1


## Recursive approach
coins <- c(1,2,5,10,20,50,100,200) # in pence
change <- 200
n <- 8 # number of coins

get_change <- function(change, n) {
    combinations <- 0
    if (n == 1) # return 1 when number of coins available is 1
        return(1)
    
    for (i in 1:n) {
        remainder <- change - coins[i] # substract coins until reaching 0
        
        if (remainder == 0) {
            combinations <- combinations + 1
        } else if (remainder > 0) {
            combinations <- combinations + get_change(remainder, i)
        }
    }
    
    return(combinations)
}


# Answer
get_change(change,n)
