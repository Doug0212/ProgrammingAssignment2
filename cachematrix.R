cachematrix <- function(x, ...) {

## The makeCacheMatrix function creates a special "matrix", which is 
## really a list containing a function to:
##   1. set the value of the matrix
##   2. get the value of the matrix
##   3. set the inverse of the matrix
##   4. get the inverse of the matrix
##
## The cacheSolve function calculates the inverse of the special "matrix"
## created using the makeCacheMatrix function. However, it first checks
## to see if the inverse of the matrix has already been calculated. If so,
## it gets the inverse of the matrix from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the matrix and
## sets the inverse of the matrix in the cache via the setsolve function.

        
## The makeCacheMatrix function creates an inverse of a matrix and
## stores it in cache so that it can be used rather than recalcuating the
## inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}



## The cacheSolve function either returns the inverse of a matrix stored
## in cache if it exists or calculates the inverse of the matrix if it is
## not already stored in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

}