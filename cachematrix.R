## Caching the Inverse of a Matrix
##
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
##
## The functions in this file provide a mechanism for caching a matrix inverse.
##
## Example of use:
##  Create some matrix      
##    m <- matrix(sample(1:10000, 10000, replace=T), 100, 100)
##
##  Create a special "caching matrix" object (really just a list)
##    cm <- makeCacheMatrix(m)
##
##  Call cacheSolve on the "caching matrix" to get the matrix inverse. The 
##  first time its called for a given matrix, it will actually compute the 
##  matrix inverse; subsequent calls will just return the cached inverse.
##    mInv <- cacheSolve(cm)


## Create a special "caching matrix", which is really just a list of functions
## to get and set the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## initialize cached inverse variable
    inv <- NULL
    
    ## define functions for getting and setting the matrix and its inverse
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inv) inv <<- inv
    getinv <- function() inv
    
    ## return the list of functions as the "caching matrix"
    list(set = set, 
         get = get,
         setinv = setinv,
         getinv = getinv)
}


## Return a matrix that is the inverse of 'x'. If the inverse has already been 
## calculated (and the matrix has not changed), then return the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
    ## get the inverse from the special "caching matrix" object
    inv <- x$getinv()
    
    ## if the inverse is not null, then return it
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## if we got to this point, then there was no cached inverse, so let's
    ## compute it, store it and return it.
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
