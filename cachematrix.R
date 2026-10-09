## These two functions cache the inverse of a matrix, so that the
## inverse is computed only once as long as the matrix does not change.

## makeCacheMatrix creates a special "matrix" object: a list of functions
## to set/get the matrix and to set/get its cached inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve returns the inverse of the special "matrix" created by
## makeCacheMatrix. If the inverse is already cached, it is returned
## directly; otherwise it is computed with solve() and stored.
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
