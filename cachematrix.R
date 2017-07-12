## Description of the Assingnment and Function

## This assignmnet is to Cawrite a pair of functions,
## namely,"makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix

## makeCacheMatrix function creates a special "matrix" objectwhich can cache inverse.

makeCacheMatrix <- function(x = matrix())
{
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setIvr <- function(inverse) inv <<- inverse
    getIvr <- function() inv
    list(set = set,
    get = get,
    setIvr = setIvr,
    getIvr = getIvr )
}

## cacheSolve function computes the inverse of the special "matrix" created by function makeCacheMatrix as programmed above.

cacheSolve <- function(x, ...)
{
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getIvr()
    if (!is.null(inv)) {
        message("Cached input data")
        return(inv)
    }
    datamat<- x$get()
    inv<- solve(datamat, ...)
    x$setIvr(inv)
    inv
}




