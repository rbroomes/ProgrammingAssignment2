## The makeCacheMatrix function creates the matrix object and caches its inverse.
## The cacheSolve function computes the inverse of the special "matrix" returned
## by makeCacheMatrix. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve retrieves the inverse from the cache.

## The makeCacheMatrix function creates the matrix object and can set/get the matrix
## as well as set/get its inverse

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
    
}


## The cacheSolve function calculates the inverse of the matrix.
## created above by makeCacheMatrix.  it first checks to see if
## the inverse has already been calculated. If so, it gets the
## inverse from the cache and skips the computation

cacheSolve <- function(x, ...) {

    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m    ## Return a matrix that is the inverse of 'x'    
        
}
