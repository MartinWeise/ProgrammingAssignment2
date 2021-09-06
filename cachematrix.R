## Caching the Mean of a Vector

## Create a matrix structure that caches its inverse representation

makeCacheMatrix <- function (x = matrix()) {
  
  inverse <- NULL
  
  # setter of matrix (remove inverse too)
  setMatrix <- function (m) {
    x <<- m
    inverse <<- NULL
  }
  
  # getter of matrix
  getMatrix <- function () {
    x
  }
  
  # setter of inverse
  setInverse <- function (i) {
    inverse <<- i
  }
  
  # getter of inverse
  getInverse <- function () {
    inverse
  }
  
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse,
       getInverse = getInverse)

}


## Compute the inverse of a matrix provided by makeCacheMatrix if
##    not already computed. If already computed, the function will
##    return the cache

cacheSolve <- function (x, ...) {
  
  inv <- x$getInverse()
  
  if (!is.null(inv)) {
    return (inv)
  }
  
  mat <- x$getMatrix()
  
  inv <- solve(mat) %*% mat
  
  x$setInverse(inv)
  
  x
  
}

#A <- matrix(c(5, 1, 0, 
#              3,-1, 2,
#              4, 0,-1), nrow=3, byrow=TRUE)
#B <- makeCacheMatrix(x = A)
#
## test getter matrix
#B$getMatrix()
#
## test setter matrix´
#B$setMatrix(A)
#B$getMatrix()
#
## test getter inverse
#B$getInverse()
#
## test cachesolve inverse
#cacheSolve(B)
#B$getInverse()
#
## test cache
#cacheSolve(B)