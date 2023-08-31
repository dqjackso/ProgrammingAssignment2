## These functions save computing resources when finding the inverse of a
## matrix. It works similar to a cache. The cache of the matrix object
## stores the inverse of the matrix. Whenever the cacheSolve function
## is called to compute the inverse of another matrix, it first checks
## to see if the cache contains an equivalent matrix with an already
## computer inverse. If so, it returns the cached inverse. If not, it
## computes the inverse and stores it in the cache.

## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  # local scoped variable to store the inverse of the matrix
  i <- NULL
  
  # Function set, search global environment for definition of variable, and
  # set it to be equal to what was passed. Then reset the local variable
  # to be NULL.
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # Function get, return the global environment object set in set()
  get <- function() x
  
  # Function setInverse, takes the inverse of the matrix as the arg and
  # sets the local i variable to be the inverse
  setInverse <- function(inverse) i <<- inverse
  
  # Function getInverse, returns the locally scoped inverse
  getInverse <- function() i
  
  # Return a list with set = set, get = get, setInverse, and getInverse
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Computes the inverse of the special matrix returned by makeCacheMatrix
## If the inverse has already been calculated & the matrix has not changed
## then cacheSolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  
  # Try to getInverse from x matrix
  i <- x$getInverse()
  
  # If x inverse is not NULL AND matrix in cache is equal to matrix passed
  if (!is.null(i)) {
    # Get "inverse of the inverse" which should be equal to the matrix passed
    m <- solve(i)
    if (all.equal(x$get(), m)) {
      # Message that you are getting it from the cache
      message('Retrieving inverse from the cache...')
      # return the inverse object from the cache
      return(i)
    }
  }
  
  # Message that inverse not cached & calculating new inverse
  message('Inverse not available in cache...')
  # Calculate inverse of the matrix variable and store it in the inverse var
  i <- solve(x$get(), ...)
  # setInverse in the cache to be equal to the inverse variable
  x$setInverse(i)
  
  ## Return a matrix that is the inverse of 'x'
  i
}
