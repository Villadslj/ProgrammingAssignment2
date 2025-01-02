## Put comments here that give an overall description of what your
## The functions cache the Inverse of a Matrix or calculates it if necessary. 

## Write a short comment describing this function
# Stores a matrix x and its inverse i and defines functions to set/get the matrix and to set/get the inverse.
# and at last returns the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}




## Write a short comment describing this function
# It checks if the inverse is already cached in the if statement. 
# If it is, it retrieves and returns the cached inverse, 
# If not, it calculates the inverse, caches it, and returns the result.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  
  if (!is.null(i)) {
    message("getting cached data")
    return(i) 
  }
  mat <- x$get()
  
  i <- solve(mat, ...)
  x$setInverse(i)
  i
}
