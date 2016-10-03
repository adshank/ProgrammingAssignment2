# The two functions below, makeCacheMatrix and cacheSolve, work together to create a special matrix object 
# that can cache its inverse, then compute and return the inverse of this special matrix.


# The makeCacheMatrix function creates a special "matrix" object that can cache its inverse
# The makeCacheMatrix function takes a matrix, x, as an input
# The makeCacheMatrix function outputs a list of four functions (set, get, setInv, getInv)
# The output is a list of four functions

makeCacheMatrix <- function(x = matrix()) {
  
  z <- NULL
  
  set <- function(y) {
    x <<- y
    z <<- NULL
  }
  
  get <- function() x
  setInv <- function(inverse) z <<- inverse
  getInv <- function() z
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}

# The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.
# The cacheSolve function returns the inverse of the matrix as an output
          
cacheSolve <- function(x, ...) {
  inverse <- x$getInv()
  
  if(!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInv(inverse)
  inverse
}
