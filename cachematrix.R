## Functions for calculating the inverse of a matrix,
## making use of the scoping rules of R to cache the result.


## Creates a matrix that will cache its inverse.
## The input must be an invertible matrix.
makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) i <<- inverse
  
  getinverse <- function() i
  
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# Calculates the inverse of a matrix object created by the 
# makeCacheMatrix function. If the inverse of the matrix has 
# already been calculated (and cached), that value will be returned. 
# If not, the inverse is calculated, cashed, and returned.
cacheSolve <- function(x, ...) {
  
  i <- x$getinverse()
  
  if(!is.null(i)) {
    message("Getting cached data")
    return(i)
  }
  
  data <- x$get()
  
  i <- solve(data, ...)
  
  x$setinverse(i)
  
  i
}