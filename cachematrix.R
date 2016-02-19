## makeCacheMatrix is a function creating a matrix object
## that may cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## cacheSolve returns the inverse of the matrix.  If
## inverse has been previously calculated and the matrix
## remains the same, it retrieves inverse from the cache.
## If not, the inverse is computed and the value is 
## moved to cache.

cacheSolve <- function(x, ...) {
  inv = x$getinv()
  if (!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  mat.data = x$get()
  inv = solve(mat.data, ...)
  x$setinv(inv)
  return(inv)
}

## Test:
## > test <- matrix(1:4, 2, 2)
## > test1 <- makeCacheMatrix(test)
## > test1$get()
##       [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > cacheSolve(test1)
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(test1)
## getting cached data.
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5