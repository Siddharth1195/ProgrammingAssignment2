## Funtions that cache the inverse of a matrix.

## Creates a matrix that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes inverse of a matrix created by makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not been changed,
## then the cacheSolve will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <-x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
  
## Example:
temp1 <- matrix(c(9,8,3,4), nrow = 2, ncol = 2)
temp2 <- makeCacheMatrix(temp1)
cacheSolve(temp2)
