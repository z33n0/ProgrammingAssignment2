## Caching the Inverse of a Matrix
## The pair of functions below store a matrix and cache its inverse rather than 
## computing it repeatedly.

## This first function, makeCacheMatrix, makes a special "matrix" object that 
## can cache its inverse and contains functions that set and get the value of 
## the matrix as well as its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ## Assume matrix is invertible
  set <- function(y){ 
    x <<- y
    inv <<- NULL
  } ## Set the value of the matrix
  get <- function() {x} ## Get the value of the matrix
  setInverse <- function(inverse) {inv <<- inverse} ## Set the value of the inverse
  getInverse <- function() {inv} ## Get the value of the inverse
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## This second function, cacheSolve, computes the inverse of the matrix created 
## with the function above (and retrieves the inverse from the cache if it has 
## already been computed).

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv) ## Set the value of the inverse in the cache
  inv ## Return a matrix that is the inverse of 'x'
}
