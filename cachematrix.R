## Function to inverse and cache the same for quick retrieval

## Caching the matrix

makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
    set <- function(y) {
        x <<- y
        invrs <<- NULL
    }
    get <- function() x
    setinverse <- function(input) invrs <<- input
    getinverse <- function() invrs
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Retrieving cached copy if exists
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invrs <- x$getinverse()
    if(!is.null(invrs)) {
        message("getting cached data.")
        return(invrs)
    }
    mat <- x$get()
    invrs <- solve(mat)
    x$setinverse(invrs)
    invrs
}
