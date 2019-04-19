## Make an cache internally the inverse matrix
## Retrieve in memory copy when exist 

## Make and cache inverse matrix

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


## Retrieve cached matrix if it exists in mem

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
