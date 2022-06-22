## Matrix inversion is usually a costly operation in terms of system resources
## and time. If the inversion of a matrix has to be calculated multiple times,
## it saves time to cache the inverse. This allows the inverse to be pulled
## from cache instead of re-calculating it every time. Two functions will be
## created: makeCacheMatrix() and cacheSolve().

## The first function will create a special "vector" that contains a list of
## functions to:
##     1. set the matrix
##     2. get the matrix
##     3. set the inverse
##     4. get the inverse
##This "vector" will be input to the cacheSolve() function.

makeCacheMatrix <- function(x = matrix()) {
  ## x must be an invertible matrix, which we will assume is the
  ## case for this assignment.
  
      MInverse <- NULL
      setinv <- function(y) {
         x <<- y
         inverse <<- NULL
      }
      get <- function()x
      setInverse <- function(inverse) MInverse <<- inverse
      getInverse <- function() MInverse
      list(setinv = setinv, get = get,
           setInverse = setInverse, getInverse = getInverse)
}

## The second function will see if the inverse matrix has already been
## calculated & cached. If it hasn't, the function will calculate the inverse
## and store it in cache. If it has been calculated, it gets the 
## inverse from cache & skips the calculation. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        MInv <- x$getInverse  ##checks for inverse in cache
        if(!is.null(MInv)) {
            messge("Getting cached data")
            return(MInv)
        }
        mdata <- x$get()
        MInv <- solve(mdata)  ##calculate the inverse
        x$setInverse(MInv)  ##sets value of MInv in cache
        return(MInv)
}

