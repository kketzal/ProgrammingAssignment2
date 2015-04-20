############################################################################
## This function make a "special matrix" with methods to SET and GET
## a "normal matrix" and his "inverted matrix". These matrices are cached. 
## Usage:
##   ----- Create an empty "special matrix" ------
##         > my_special_matrix <- makeCacheMatrix()
##   ----- Create a "special matrix" with a previously created matrix "my_matrix" ------
##         > my_special_matrix <- makeCacheMatrix(my_matrix)
##
## Return: a list with the Set and Get functions
###########################################################################
makeCacheMatrix <- function(my_matrix = matrix()) {
        
        # setting initial value in this environment
        my_inverted_matrix <- NULL
        
        ## Set a normal matrix ("my_matrix") and initializing "my_inverted_matrix". 
        ## Both in other environment or "caching" (using '<<-' operator).
        ##      Usage:       
        ##              my_special_matrix$set_matrix(x)
        ##-----------------------------------------------
        set_matrix <- function(my_matrix) {               
                my_matrix <<- my_matrix                
               # my_inverted_matrix <<- NULL
        }
        
        ##  Get the normal matrix ("my_matrix")
        ##      Usage:      
        ##          my_special_matrix$get_matrix()
        ## ----------------------------------------------
        get_matrix <- function() my_matrix
        
        
        ##  Set the inverted matrix "inverted_matrix"       
        ##      Usage:      
        ##          my_special_matrix$set_inverted_matrix(inverted_matrix)
        ## ----------------------------------------------
        set_inverted_matrix <- function(inverted_matrix) {
            my_inverted_matrix <<- inverted_matrix
        }
        
        ##  Get the inverted matrix "inverted_matrix"       
        ##      Usage:      
        ##          my_special_matrix$get_inverted_matrix()
        ## ----------------------------------------------
        get_inverted_matrix <- function() my_inverted_matrix
        
        
        ##  return a List with the get and set functions
        list(set_matrix = set_matrix, 
             get_matrix = get_matrix,
             set_inverted_matrix = set_inverted_matrix,
             get_inverted_matrix = get_inverted_matrix)
}



###########################################################################
## This function computes the inverted matrix for a given squared matrix.
## To work properly, the argument "my_special_matrix" must be an object 
## created with the "makeCacheMatrix" function.
## Usage:
##         > cacheSolve(my_special_matrix)
##
## Return: show the inverted matrix, or an error if the matrix isn't a 
##         squared matrix. 
##         The inverted matrix is cached inside the "my_special_matrix" object
###########################################################################
cacheSolve <- function(my_special_matrix, ...) {
        
        ## get the inverted matrix of the argument object "my_special_matrix",
        ## if it exists...
        my_inverted_matrix <- my_special_matrix$get_inverted_matrix()
        
        ## If inverted matrix had been computed, return cached data..
        if(!is.null(my_inverted_matrix)) {
                message("getting cached data...")
                return(my_inverted_matrix)
        }
        
        ## ELSE... 
        ## 1st: -->  get the matrix data 
        my_matrix <- my_special_matrix$get_matrix()
        
        ## 2nd: --> check if the matrix is squared: "num cols == num. rows..."
        ## if not, stop the execution 
        if(ncol(my_matrix) != nrow(my_matrix)) {                
                stop("Matrix isn't squared, so it's not invertible.")
        }
                
        ## 3th: --> computting the inverse and storing in cache
        message("computting inverted matrix...")
        my_inverted_matrix <- solve(my_matrix)
        
        message("caching inverted matrix...")
        my_special_matrix$set_inverted_matrix(my_inverted_matrix)
        
        ## Show the inverted matrix...
        my_inverted_matrix
        
}