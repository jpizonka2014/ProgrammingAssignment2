## This set of functions handles inverting square matrixes.  Performance
## considerations are made by caching the compute intensive operation.
## NOTE: This does not handle the case where the makeCacheMatrix is called
## with one matrix and cacheSolve is called with a different matrix.

### This method enhances the matrix object by allowing it to cache itself
### and thus giving the end user a more performant matrix inversion
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


### This function returns the inverse of X assuming that is a square matrix
### and has not changed.  first attempts to retrieve a cached version.  if that 
### doesn't exist, then it calculates the inverse and saves it
cacheSolve <- function(x, ...) {
    #### Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m  
}
