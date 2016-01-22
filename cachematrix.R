##  The two functions below are used to create a special object that stores a matrix 
##  and caches its inverse.

## mmakeCacheMatrix creates a special "vector", which is really a list containing a function to

## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of matrix
## get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  matinv <- NULL
  set <- function(y) {
    x <<- y
    matinv <<- NULL
  }
  get <- function() x
  setmatinv <- function(inv) matinv <<- inv
  getmatinv <- function() matinv
  list(set = set, get = get,
       setmatinv = setmatinv,
       getmatinv = getmatinv)
  
}


## The following function calculates the inverse of the special "cache matrix" 
## created with the above function. However, it first checks to see 
## if the inverse matrix has already been calculated. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, it calculates 
## the inverse of the matrix and sets the value of the inverse in the cache 
## via the setmatinv function.

## The argument x to the cachesolve function should be the list returned  by the 
## makecachematrix function.

cacheSolve <- function(x,...) {
  matinv <- x$getmatinv()
  if(!is.null(matinv)) {
    message("getting cached data")
    return(matinv)
  }
  data <- x$get()
  matinv <- solve(data, ...)
  x$setmatinv(matinv)
  matinv
}
