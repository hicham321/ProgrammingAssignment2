## These functions take a matrix X as an input and compute the inverse 
## of a matrix and then cache it, if the inverse matrix is already 
## computed for some input it should return the already computed value.  

## makeCacheMatrix creates a matrix and stores the value of the inverse
## in another environment using lexical scoping

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  #scoping function for the input x:
  get <- function() x
  
  #setter and getter functions for the inverse
  setinverse <- function(matrixinverse) inverse <<- matrixinverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSlove checks if the matrix inverse is computed and returns 
## the computed value, if not it calculates the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  #testing if the inverse already exists for the input 
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  #getting the data and computing the inverse
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
