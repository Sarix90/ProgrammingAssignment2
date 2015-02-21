# The purpose of the assignment is to create two functions. One that creates a matrix
# whose inverse can be cached, and second one that determines the inverse matrix or checks
# if the inverse has been cached. 

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


## This function checks if a matrices inverse has been cached and returns it.
## If it hasn't been cached, it determines the inverse of the matrix "x" 
cacheSolve <- function(x, ...) {
  
  #see if inverse matrix has been cached 
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
