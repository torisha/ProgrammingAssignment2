
## makeCacheMatrix() returns a list containing functions to
##  1. set the matrix
##  2. get the matrix
##  3. set the inverse
##  4. get the inverse
## this list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
## set matrix
set <- function(y) {
    x <<- y
    inv <<- NULL
}
## get matrix
get <- function() x
## set and get the inverse
set_inv <- function(solve) inv <<- solve 
## the '<<-' operator is used for assigning to variables in the parent environments
get_inv <- function() inv
list(set=set, get=get, set_inv=set_inv, get_inv=get_inv)

}



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    s <- x$get_inv() 
    if(!is.null(s)) {
        message(">>> getting cached data")
        return(s)
    }
    else{
        message(">>> computing")
        data <- x$get()
        s <- solve(data, ...)
        x$set_inv(s)
        return(s)
    }
    
}
