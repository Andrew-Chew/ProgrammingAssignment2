# Coursera
# R Programming
# Assignment 2

# Matrix inversion is usually a costly computation. Therefore, the following
# functions enable this computation to be skipped if it has already been done 
# on a matrix by caching the result.

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # Variable to cache inverse.
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

# This function compute the inverse of the special "matrix" returned by the
# above function if it hasn't already been computed and the matrix is unchanged.
# Otherwise, retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  # Get inverse of matrix x.
  i <- x$getinv()
  
  # Check if inverse has already been computed, return cached data if it is.
  if (!is.null(i)) {
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