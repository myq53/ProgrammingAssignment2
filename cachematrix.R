

makeCacheMatrix <- function(x = matrix()) {
## For use in caching the inverse of a matrix; returns a list consisting of functions to
## set and get the values of the matrix to be inverted and to set and get the inverse.
    I <- NULL
    set <- function(y) {
        x <<- y
        I <<- NULL
    }
    get <- function() x
    setinv <- function(inv) I <<- inv
    getinv <- function() I
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}




cacheSolve <- function(z, ...) {
## Computes the inverse of the matrix, using caching from makeCacheMatrix
## Input should be z=makeCacheMatrix(x) where x is the matrix to be inverted
    I <- z$getinv()
    if(!is.null(I)) {
        message("getting cached data")
        return(I)
    }
    data <- z$get()
    I <- solve(data, ...)
    z$setinv(I)
    I
}
