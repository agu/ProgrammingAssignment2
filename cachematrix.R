## The following functions' purpose is to decrease computational cost of 
##   matrix inversion by caching the inverse of matrix (in order to look it up
##   in the cache instead of computing it again when needed again).

## function "makeCacheMatrix" creates a special "matrix" object 
##   that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get, 
       setsolve = setsolve,
       getsolve = getsolve)
}


## function "cacheSolve" computes the inverse of the special "matrix" object
##   (returned by the first function), only when it was not computed earlier
##   (in other case - it returns the inverse from the cache)

cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
      message("getting cached data")
      return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    ## Return a matrix that is the inverse of 'x'
    s
}
