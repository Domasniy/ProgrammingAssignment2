## Put comments here that give an overall description of what your
## functions do
## This functions solve the inverse of a matrix, and put result to cache.



## This function describe functions for working with matrix.
## Set or get input matrix, and Set, Get function for inverse one.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
## This function checking for matrix changed or not, if its old one,
## function return cache version of inverse matrix, if its changed, then
## solving inverse matrix and put it to cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
