## 1. makeCacheMatrix: This function creates a special "matrix" object that can
## cache its inverse.
makeCacheMatrix <- function(m = matrix()) {
        # initialize the inverse matrix
        i <- NULL
        
        # set the value of the matrix
        set <- function(y) {
                m <<- y
                i <<- NULL
        }
        
        # get the value of the matrix
        get <- function() m
        
        # set the value of the inverse matrix
        setinverse <- function(solve) i <<- solve
        
        # get the value of the inverse matrix
        getinverse <- function() i
        
        # return a list of the functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## 2. cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache.
cacheSolve <- function(m, ...) {
        # set the inverse matrix of m
        #m <- x$getinverse()
        i < m$getinverse()
        
        # if cached data found return cached data
        if(!is.null(i)) {
                message("Getting cached data")
                return(i)
        }
        ## when no cached data is found, compute and return inverse matrix
        data <- m$get()
        i <- solve(data, ...)
        m$setinverse(i)
        i
}
