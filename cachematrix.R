## Implements functions to get the inverse of a matrix and cache results

## Returns a list with getters and setters to be used with cacheSolve

makeCacheMatrix <- function(x = matrix()) {
        inversematrix <- NULL
        set <- function(y) {
                x <<- y
                inversematrix <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inversematrix <<- inverse
        getinverse <- function() inversematrix

        list(
             set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse
        )
}


## Receives a matrix created with makeCacheMatrix and returns the inverse. Uses
## cached result if able.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        inverse <- solve(x$get(), ...)
        x$setinverse(inverse)
        inverse
}
