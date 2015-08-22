## This function takes inverse of a matrix and stores in the cache
## During further matrix-inverse operation it looks at the cache
## and if there is a value of matrix it skips the inverse operation

## makeCasheMatrix function stores list of functions where matrix inversion is computed and storeed
## This function can be storred in an object to be called in the following function "cacheSolve"

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        set.solve <- function(solve) m <<- solve
        get.solve <- function() m
        list(set = set, get = get, 
             set.solve = set.solve, 
             get.solve = get.solve)
}

## cacheSolve looks for the storred inverse value in the previous function: maCacheMatrix and if m is not-Null it provides the statement "getting cached data", skips inverse computation and reports the inverse matrix.
## if m is null, then the cacheSolve function computes the inverse value of the matrix and reports the inverse matrix.

cacheSolve <- function(x = matrix(), ...) {
        m <- x$get.solve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m<- solve(data, ...)
        x$set.solve(m)
        m
}
