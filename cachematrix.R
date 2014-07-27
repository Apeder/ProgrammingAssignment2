## makeCacheMatrix and cacheSolve allow a program to avoid repeatedly
## caculating a matrix' inverse by caching the value of matrices' 
## inverses and allowing functions to access these values throughout
## the global environment. 
 

## makeCacheMatrix makes a list containing the functions to set the value of the 
## matrix, retrieve the value of the matrix, calculate the value of 
## the matrix' inverse and retrieve the value of the matrix' inverse. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL 
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, 
            setinverse = setinverse, 
            getinverse = getinverse)
}

## This function caculates the inverse of the matrix created by
## makeCacheMatrix, and if the inverse was previously calculated,
## it retrieves it from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
