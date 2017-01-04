## This pair of functions cache the inverse of a matrix. 

## makeCacheMatrix is a function that creates a special 
## "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(solveMatrix) inv <<- solveMatrix
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, 
             getInverse = getInverse)
}


## cacheSolve is a function that calculates the inverse
## of the "matrix" created by makeCacheMatrix. First, it
## determines if the inverse has already been calculated. 
## If the inverse is already saved in the cache, the function 
## retrieves the saved calculation. Otherwise, cacheSolve
## calculates the inverse of the "matrix" and saves it to 
## the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}