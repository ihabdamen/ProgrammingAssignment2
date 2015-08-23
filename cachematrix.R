## This Script includes functions that creates a matrix special object and provides the inverse of the 
## matrix, the script also caches the matrix in order to fetch the inverse without 
## recalculating it in future calls

## This function creates a special object that encapsulate all the needed functions and 
## attributes of the matrix  to be inversed

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inv <<- solve
  getsolve <- function() inv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function calculates the inverse of the provided matrix special object and 
## caches the inverse of the matrix

cacheSolve <- function(x, ...) {
  inv <- x$getsolve()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setsolve(inv)
  inv
}
