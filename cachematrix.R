## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is used to create a special matrix, which is a list containing some functions
## set: set value to matrix (x) and inverted matrix (m)
## get: get value
## setInverse: set the inverted matrix to the variable m
## getInverse: get the inverted matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(y) m <<- y
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve is used to calculate the inverted matrix. In a case that the inverted matrix is
## already calculated, this function will get the value from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
