##--------##---------##--------##---------##--------##---------##--------##
## This function make a "special matrix" with methods to SET and GET
## a "normal matrix" and his "inverted matrix". The matice##are stored 
## Usage:
##   ----- Create an empty "special matrix" ------
##
##          my_special_matrix <- makeCacheMatrix()
##
###------##--------##---------##--------##---------##--------##---------##

##   ----- Set a normal matrix "x" to my "special matrix" ------
##          
##          my_special_matrix$set_matrix(x)
##
##   ----- Get the normal matrix "x" from my "special matrix" ------
##
##          my_special_matrix$get_matrix()

makeCacheMatrix <- function(x = matrix()) {
        
        inverted_matrix <- NULL
        
        set_matrix <- function(y) {
                x <<- y
                inverted_matrix <<- NULL
        }
        get_matrix <- function() x
        set_inverted_matrix <- function(inv_mat) inverted_matrix <<- inv_mat
        get_inverted_matrix <- function() inverted_matrix
        
        list(set_matrix = set_matrix, 
             get_matrix = get_matrix,
             set_inverted_matrix = set_inverted_matrix,
             get_inverted_matrix = get_inverted_matrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}