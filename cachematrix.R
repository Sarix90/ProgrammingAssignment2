## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix whose inverse can be cached

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # caches unaltered matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # gets unaltered matrix
  get <- function() x
  
  # caches inverse matrix
  setInverseMatrix <- function(solve) m <<- solve
  
  # gets inverse matrix
  getInverseMatrix <- function() m
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  
  
  m <- x$getInverseMatrix ()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(matrix, ...)
    x$setInverseMatrix(m)
    m
}
