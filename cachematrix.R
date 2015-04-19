############################################################################
## This function make a "special matrix" with methods to SET and GET
## a "normal matrix" and his "inverted matrix". These matrices are cached. 
## Usage:
##   ----- Create an empty "special matrix" ------
##         > my_special_matrix <- makeCacheMatrix()
##   ----- Create a "special matrix" with a previously created matrix "my_matrix" ------
##         > my_special_matrix <- makeCacheMatrix(my_matrix)
###########################################################################
makeCacheMatrix <- function(my_matrix = matrix()) {
        # setting initial value in this environment
        my_inverted_matrix <- NULL
        
        ## Set a normal matrix "my_matrix" and initializing "my_inverted_matrix". 
        ## Both in other environment or "caching" (using '<<-' operator).
        ##      Usage:       
        ##              my_special_matrix$set_matrix(x)
        ##-----------------------------------------------
        set_matrix <- function(my_matrix) {               
                my_matrix <<- my_matrix                
                my_inverted_matrix <<- NULL
        }
        
        ##  Get the normal matrix "my_matrix"       
        ##      Usage:      
        ##          my_special_matrix$get_matrix()
        ## ----------------------------------------------
        get_matrix <- function() my_matrix
        
        
        set_inverted_matrix <- function(inverted_matrix) {
            my_inverted_matrix <<- inverted_matrix
        }
        
        get_inverted_matrix <- function() my_inverted_matrix
        
        list(set_matrix = set_matrix, 
             get_matrix = get_matrix,
             set_inverted_matrix = set_inverted_matrix,
             get_inverted_matrix = get_inverted_matrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}