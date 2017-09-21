## These functions compute the inverse of a matrix. The computation is skipped
## if the matrix's inverse is already cached.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, 
       get = get, 
       setinv = setinv, 
       getinv = getinv)
}



## This function computes the inverse of the matrix created by the function above.
## If the inverse is cached, this function pulls it from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
