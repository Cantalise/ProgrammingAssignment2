## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # This function creates a special "matrix" object that can cache its inverse
  inv <- NULL
  set <-function(y) {
    x <<- y
    # inverse of the matrix
    inv <<- matrix(data=NA, nrow=ncol(x), ncol=nrow(x))
  }
  
  # get the matrix
  get <- function() x
  
  # store the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  
  # get the inverse of the matrix
  getinverse <- function() inv
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    
    # check whether the inverse matrix has been cached. If so, return it. 
    # If not, compute it using solve function.
    if(!is.null(inv)) {
      message("getting cached inverse matrix")
      return(inv)
    }
    
    data <- x$get()
    inv <- solve(data,...)
    x$setinverse(inv)
    inv
}
