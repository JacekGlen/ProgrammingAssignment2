## Matrix inversion is usually a costly computation and there may be some benefit to caching
## the inverse of a matrix rather than computing it repeatedly (there are also alternatives 
## to matrix inversion that we will not discuss here). 
## The following two helper functions can be used to cache the inverse of the matrix.



# This function creates a special "matrix" object that can cache its inverse. It returns a list of functions:
# 1. set(x): set the value of the matrix
# 2. get(): get the value of the vector
# 3. setInverse(inverse): set the value of the inverse
# 4. getInverse(): get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) i <<- inverse
  
  getInverse <- function() i
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cacheSolve  retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  # Returns a matrix that is the inverse of 'x'

  # get cached inverse  
  inverse <- x$getInverse()
  
  # if exists, return cached results
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  # else, calculate and store inverse, then return the results
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  
  inverse
}
