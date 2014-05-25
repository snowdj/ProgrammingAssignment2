## Matrix inversion is usually a costly computation and their may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly
## cachematrix.R creates a special "matrix" object that can cache its inverse.


## Usage:
## M <- matrix(1:4, nrow=2, ncol=2)
## cacheMatrix <- makeCacheMatrix(M)
## cacheSolve(cacheMatrix)


makeCacheMatrix <- function(x = matrix()) {
        # 'cachedInverse' variable will store the cached inverse of the matrix
        # Initialize 'cachedInverse' is null
        cachedInverse <- NULL
        # 'set' function used to set the value of input matrix
        set <- function(y) {
                x <<- y
                cachedInverse <<- NULL
        }
        # get' function  used to get the value of input matrix
        get <- function() x
        # 'setInverse' function used to cache the value of inverse
        setInverse <- function(inverse) cachedInverse <<- inverse
        # 'getInverse' function used to get the cached value of inverse
        getInverse <- function() cachedInverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                
        # Get cached value of the inverse
        invFunc <- x$getInverse()
        # If invFunc is not NULL, then the input matrix has not changed and the inv
        # contains the cached value of the matrix, then return inv        
        
        if(!is.null(invFunc)) {
                message("getting cached data")
                return(invFunc)
        }
        # If invFunc is NULL, get the new value of the input matrix for calculating inverse
        data <- x$get()
        # Solve the matrix for inverse and save the value of inverse in 'invFunc'
        invFunc <- solve(data, ...)
        # Cache the new value of inverse        
        x$setInverse(invFunc)
        # Return the updated value of inverse
        invFunc
}
