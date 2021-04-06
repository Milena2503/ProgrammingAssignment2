##Assignment R programming Week 3 - Milena 
##In order to prevent that program has to compute the matrix inversion
## repeatedly, we can apply some cache functions
##
## The code below consists of two functions: makeCacheMatrix & cacheSolve

## makeCacheMatrix is the functions that cache the inverse of some matrix

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
          x <<- y
          inv <<- NULL
      }
      get <- function() {x}
      setInverse <- function(inverse) {inv <<- inverse}
      getInverse <- function() {inv}
      list(set = set, get = get, setInverse = setInverse, getInverse=getInverse)
}


## cacheSolve is the function that retrieves already calculated matrix inverse
## from cache

cacheSolve <- function(x, ...) {
  inv <-x$getInverse()
  if(!is.null(inv)){
    message("get cached data")
    return(inv)
  }
  mx <- x$get()
  inv <- solve(mx, ...)
  x$setInverse(inv)
  inv
}
