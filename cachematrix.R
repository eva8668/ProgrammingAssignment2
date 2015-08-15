###############################################################################
## These pair of functions are to calculate and cache the inverse of matrix. ##
###############################################################################

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        ## The inverse of the matrix
        cacheInverse <- NULL
        
        ## Define functions to access the matrix and inverse cache
        set <- function(y) {
                x <<- y
                cacheInverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) cacheInverse <<- inv
        getInverse <- function() cacheInverse
        
        ## Return a list containing the above functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## This function computes or retrieves the inverse of the special "matrix"
## returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
        ## Check if the inverse has already been calculated
        inv <- x$getInverse()
        if (!is.null(inv)) {
                ## message("getting cached data")
                return(inv)
        }
        
        ## If no, compute and return a matrix that is the inverse of 'x'
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
