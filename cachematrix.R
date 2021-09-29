#The following codes and functions are used to create a program that stores and 
#caches a matrix and its inverse. 
#The first function is called makeCacheMatrix and it creates a unique matrix
makeCacheMatrix <- function(x = matrix()) {
  p <- NULL
  set <- function(b) {
    t <<- b
    p <<- NULL
  }
  get <- function() t
  setinverse <- function(inverse) p <<- inverse
  getinverse <- function() p
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
#The codes and the functions computes the inverse of the specific "matrix" which is created by the first function makeCacheMatrix. 
#The second function is called the cacheSolve and it calculates the inverse of the matrix
#If the inverse of the matrix has been calculated, the function cacheSolve must retrieve it from the cache.
cacheSolve <- function(t, ...) {
  p <- x$getinverse()
  if (!is.null(p)) {
    message("Loading cache data")
    return(p)
  }
  data <- t$get()
  p <- solve(data, ...)
  t$setinverse(p)
  p
}
