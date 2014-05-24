## Functions to allow the inverse of a matrix to be cached in order 
## eliminate unneccesary processing time.

## This function creates a matrix object which can cache its own inverse.

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL

set <- function(y){
    x <<- y
    inv <<- NULL
}

get <- function() x

setinverse <- function(inverse) inv <<- inverse

getinverse <- function() inv

list(set = set, get= get,
     setinverse = setinverse,
     getinverse = getinverse)
}

## This function returns the inverse of the matrix above using the cached
## value if present.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("using cached data")
        inv
    }
    else{
        message("calculating new inverse")
        mtrx <- x$get()
        inv <- solve(mtrx, ...)
        x$setinverse(inv)
        inv 
    }
}
