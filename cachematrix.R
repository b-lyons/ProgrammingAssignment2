##  Assignment 2 for R Programming
#  These programs use lexical scoping to allow us to use
# a matrix which caches it's inverse.

##  makeCacheMatrix creates a special "matrix", which is really 
#  a list containing functions to
#  get / set the matrix 
#  get / set the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(xinv) inv <<- xinv
        getinv <- function() inv
        list(set = set, get = get,
             setmean = setinv,
             getmean = getinv)
}


## This function returns the cached inverse if 
## available or otherwise calculates it, caches it and returns it
cacheSolve <- function(x, ...) {
        xinv <- x$getmean()
        if(!is.null(xinv)) {
                message("getting cached data")
                return(xinv)
        }
        data <- x$get()
        xinv <- solve(data, ...)
        x$setmean(xinv)
        xinv
}
