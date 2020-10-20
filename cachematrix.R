## Put comments here that give an overall description of what your
## functions do
## cacheSolve finds the inverse of a matrix and stores the inverse to cache, 
## taking the argument makeCacheMatrix(m), where m is a square matrix. It also
## its check to see if the inverse is already stored in cache before calculating.

## Write a short comment describing this function
## makeCacheMatrix creates the functions needed to pass a matrix, 'x', through
## cacheSolve to calculate the inverse of 'x'. 'i' is the Inverse matrix of 'x'.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) i <<- solve
        getsolve <- function() i
        
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve first checks to see if the inverse of 'x' is stored in the cache, 
## then if not it proceeds to calculate the inverse of 'x' and stores that in
## the cache. 
## 'x' must be given in the form makeCacheMatrix(m), where m is a square matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getsolve()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setsolve(i)
        i
}
