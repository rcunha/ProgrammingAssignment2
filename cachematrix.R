## Assignment: Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  setminv <- function(mi) minv <<- mi
  getminv <- function() minv
  list(set = set, get = get,
       setminv = setminv,
       getminv = getminv)
}

## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    minv <- x$getminv()
    if(!is.null(minv)) {
      message("getting cached data")
      return(minv)
    }
    data <- x$get()
    minv <- solve(data, ...)
    x$setminv(minv)
    minv
}
