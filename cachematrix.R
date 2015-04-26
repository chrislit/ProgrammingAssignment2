## Put comments here that give an overall description of what your
## functions do

## Create a matrix that cacehes its inverse
## returns a list consisting of functions to:
### set the value of the matrix
### get the value of the matrix
### set the value of the matrix's inverse
### get the value of the matrix's inverse

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


## Determine a matrix's inverse, pulling from the cache if it is available
## or solving if not.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  # check for cached matrix
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }

  # otherwise, calculate & set inverse before returning
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
