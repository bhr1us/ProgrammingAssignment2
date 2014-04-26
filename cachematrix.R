## These functions cache the inverse of a matrix

## The makeCacheMatrix function creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve(x)
    getinverse <- function() m
    list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The catcheSolve function computes the inverse of the matrix 
## returned by makeCatchMatrix.  If the inverse is already calculated,
## than catchSolve retrives the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
