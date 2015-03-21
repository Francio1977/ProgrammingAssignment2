## Functions for computing the inverse of a matrix, storing it in the cache 
## memory and loading it from the cache memory for subsequent inversions without new
## computations.

## Create a special matrix starting from the matrix 'x', that is actually a list 
## of functions for setting and getting the cache values of the matrix itself and 
## of its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Return the inverse of the matrix 'x'. The inverse matrix is loaded from cache if 
## it has been already computed.

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