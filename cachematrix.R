## This file contains 2 functions for efficient matrix inverse computation:
## makeCacheMatrix: creates a special "matrix" object that can
## cache its inverse.
## cacheSolve: computes the inverse of a makeCacheMatrix funcion, returning
## the cached value if the inverse was already computed and the matrix has
## not changed.

## makeCacheMatrix: creates a special "matrix" object that can
## cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  ## function to set the matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  ## function to get the matrix
  get <- function() x
  
  ## function to set the inverse
  setinverse <- function(inv) inverse <<- inv
  
  ## function to get the inverse
  getinverse <- function() inverse
  
  # return a list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: computes the inverse of a makeCacheMatrix funcion, returning
## the cached value if the inverse was already computed and the matrix has
## not changed.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## check for cached inverse
  i <- x$getinverse()
  if(!is.null(i)) {
    ## found cached inverse, return and exit
    message("getting cached data")
    return(i)
  }
  
  ## no cached inverse, so must compute it
  m <- x$get()
  i <- solve(m)
  x$setinverse(i)
  i
}
