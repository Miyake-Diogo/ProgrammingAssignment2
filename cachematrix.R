## Put comments here that give an overall description of what your
## functions do
## 1.  `makeCacheMatrix`: This function creates a special "matrix" object
## that can cache its inverse.
## 2.  `cacheSolve`: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

## Function to inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inversed <- NULL
  
  set <-function(y) {
    x <<- y
    inversed <<- NULL
  }
  
  get <- function() x
  setInversed <- function(newInversed) inversed <<- newInversed
  getInversed <- function() inversed
  list(set = set, get = get, setInversed = setInversed, getInversed = getInversed)
}



## Function to solve Matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inversed <- function(x, ...) {
    inversed <- x$getInversed()
    if(!is.null(inversed)) {
      message("getting cached Matrix")
      return(inversed)
    }
    data <- x$get()
    inversed <- solve(data, ...)
    x$setInversed(inversed)
    inversed
}
}