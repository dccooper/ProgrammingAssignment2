## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ## set the value of the matrix
  m = NULL
  set <- function(y) {
    
    # '<<-' is used to set the value of an object that is different from the current environment
    x <<- y
    m <<- NULL
  }
  
  ## get and set the value of the matrix
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getmean()
  
  # First check to see if the inverese hasalready been calculated
  if(!is.null(m)) {
    
    # If it has, return the value from the cache without further computation
    message("getting cached data")
    return(m)
  }
  
  # If not, go ahead and run the calculation
  data <- x$get()
  m <- solve(data, ...)
  
  # And set the inverse
  x$setInverse(m)
  m  
}
