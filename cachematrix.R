## makeCacheMatrix creates a matrix which contains 
## functions to do the following:
## 1. set the matrix 
## 2. get the matrix
## 3. set the inversion of the matrix
## 4. get the inversion of the matrix

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) s <<- solve
        getSolve <- function() s
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)

}


## cacheSolve takes a matrix as input and returns inversion of that matrix.
## It checks the cached inversion when first called. If the inversion was
## found, it returned the cached data. Otherwise it will execute solve function
## to produce the inversion and save it with x$setinv for future use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of matrix 'x'
        s <- x$getSolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setSolve(s)
        s
}
