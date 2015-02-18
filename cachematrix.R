## makeCacheMatrix and cacheSolve functions computes the inverse of a
## matrix and save the value for future uses.

## makeCacheMatrix creates a matrix that contains 4 inner functions:
##     set: Replace the initial cache matrix.
##     get: Return the current cache matrix.
##     setinv: Set the inverse of the cache matrix.
##     getinv: Return the current cache inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## cacheSolve retrieves or computes the inverse of a makeCacheMatrix
## matrix, this depends whether the value it's already saved or not.

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
