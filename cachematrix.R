## The functions in this file cache the inverse of a matrix when it is calculated,
## so to be retrieved later on without duplicate calculation.

## This function stores a matrix, can cache the inverse of the matrix if the
## method setinverse is called, and can output the cache if getinverse is called

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function directly returns the inverse of the matrix x if already cached,
## otherwise calculates and cache's it before returning

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
