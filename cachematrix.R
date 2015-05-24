## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly. These functions are used to cache the inverse of a matrix.


## makeCacheMatrix creates a list to get and set the value of the matrix, as
## well as get and set the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    inverse_matrix <- NULL

    set <- function(y) {
        x <<- y
        inverse_matrix <<- NULL
    }
    
    get <- function() x
    
    set_inverse <- function(inverse) inverse_matrix <<- inverse
    
    get_inverse <- function() inverse_matrix
    
    list(set = set, get = get, set_inverse = set_inverse, 
         get_inverse = get_inverse)
}


## cacheSolve returns the inverse matrix of a given matrix x. It first checks if
## the inverse has already been calculated and if so, returns the inverse from
## the cache. Otherwise, it calculates the inverse matrix and sets the value of
## the inverse in the cache using the set_inverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse_matrix <- x$get_inverse()
    
    if(!is.null(inverse_matrix)) {
        message("getting cached data")
        return(inverse_matrix)
    }
    
    data <- x$get()
    inverse_matrix <- solve(data, ...)
    x$set_inverse(inverse_matrix)
    
    inverse_matrix
}










