# Matrix inversion is usually a costly computation. The function below  and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly
# (there are also alternatives to matrix inversion that we will not discuss here).
# Your assignment is to write a pair of functions that cache the inverse of a matrix.
# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  # Function to set value of matrix.
  set <- function(m) {
    x <<- m
    i <<- NULL
  }
  
  # Function to get value of matrix.
  get <- function() {
    x
  }
  
  # Function to set value of inverse.
  setinv <- function(inv) {
    i <<- inv
  }
  
  # Function to get value of inverse.
  getinv <- function() {
    i
  }
  
  # Return special "matrix".
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix above. If the inverse has already been calculated
# (and the matrix has not changed), then the cachesolve should retrieve the
# inverse from the cache.
cacheSolve <- function(x, ...) {
  # Get inverse of matrix x.
  i <- x$getinv()
  
  # Check if inverse has already been computed.
  if (!is.null(i)) {
    # Return cached data.
    message("Getting cached data.")
    return(i)
  }
  
  # Else, compute inverse of matrix and cache it.
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  
  # Return computed inverse.
  i
}
