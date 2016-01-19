## The following functions provide a method for creating
## and caching a CacheMatrix.  A CacheMatrix is used for
## calculating and storing the inverse of provided matrix.

## The makeCacheMatrix() function stores a given matrix
## as well as calculates the inverse of the matrix.
## The inverse calculation is not stored until directed
## using the setinverse() function.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  get <- function(){
    x
  }
  
  setinverse <- function(minverse){
    m <<- solve(x)
  }
  
  getinverse <- function(){
    m
  }
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve() function checks if the inverse matrix has
## already been calculated.  If already calculated, the value
## will be returned from cache.  Otherwise, the value will
## be calculated.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  x$setinverse(data)
  m <- x$getinverse()
  m
}

