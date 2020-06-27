## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# The inv variable stores value of inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  # Set matrix value
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  # Set inverse matrix
  setinv <- function(solve) inv <<- solve
  
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  # Get matrix and computing the inverse of matrix
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
