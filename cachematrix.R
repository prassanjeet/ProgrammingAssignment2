## Creates a special matrix object that encapsulates the content
## and provide cached inverse of the matrix

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()){
        inverse_m <- NULL
        set <- function(y){
                x <<- y
                inverse_m <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse_matrix) inverse_m <<-inverse_matrix
        get_inverse <- function() inverse_m
        list(set = set, get = get, 
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## cacheSolve computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse_m <- x$get_inverse()
        if(!is.null(inverse_m)){
                message("getting cached inverse matrix")
                return(inverse_m)
        }
        data <- x$get()
        inverse_matrix <- solve(data,...)
        x$set_inverse(inverse_matrix)
        inverse_matrix
}
