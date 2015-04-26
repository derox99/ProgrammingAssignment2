
# With these two functions you can compute the inverse of a matrix efficiently
# makeCacheMatrix adds the capability to cache the inverse of your matrix
# cacheSolve calculates the inverse of the matrix created with makeCacheMatrix


# creates an 'object' that contains the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# Returns the inverse of the matrix x computeted in efficient way,
# if it is already present in cache the calculation will be skipped
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
