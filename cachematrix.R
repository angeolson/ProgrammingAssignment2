## These functions seek to eliminate repeated calculation of the inverse of a given matrix, which can be a costly computation


## This function creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = numeric(), row = numeric(), col = numeric()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() matrix(x, row, col)
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix. If the inverse has already been calculated, this function retrieves the inverse from the cache. 

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  ## Return a matrix that is the inverse of 'x'
  inv
}



