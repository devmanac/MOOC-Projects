## Computing inverse of a matrix can be time consuming and processor intensive
## Here we are trying to return the inverse of a matrix if it is available in cache
## On first call the inverse is calculated and stored in cache which could be retrieved
## on subsequent calls.
## FOr this the function makeCacheMatrix creates a special matrix from our input matrix
## We could use this special matrix and call the cacheSolve function to compute the inverse of matrix

## This function, makeCacheMatrix creates a special "matrix", which is a list containing a function to
# set the matrix
# get the matrix
# set the inverse of the matrix
# get the inverse of the matrix
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


## This function, cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix. 
# If the inverse has already been calculated, then the cachesolve returns the inverse from the cache.
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