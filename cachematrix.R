## This file contains a set of two functions that provide
## the possibility of caching both a matrix and its inverse. 

## Initially stores the matrix passed as the parameter and sets
## the inverse matrix it is able to cache to null.
## Provides getter and setter access to both these fields.
##
## The default parameter value provided in the original file
## has been removed because it is not invertible and leads to errors 
## when the inverse is requested through the cacheSolve function. 
makeCacheMatrix <- function(original) {
      inverse <- NULL
      set <- function(neworiginal) {
            original <<- neworiginal
            inverse <<- NULL
      }
      get <- function() original
      setinverse <- function(newinverse) inverse <<- newinverse
      getinverse <- function() inverse
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## Returns the inverted matrix of the matrix cached in the makeCacheMatrix 
## function passed as a parameter.
## It first tries to get the cached inverse matrix that the parameter 
## function may contain. When that fails it calculates the inverse by 
## using the solve() function, caches it in the parameter function 
## and returns it.
cacheSolve <- function(x, ...) {
      inverse <- x$getinverse()
      if(is.null(inverse)) {
            message("no cached inverse, calculating and caching it.")
            original <- x$get()
            newinverse <- solve(original)
            x$setinverse(newinverse)
      }
      x$getinverse()
}
