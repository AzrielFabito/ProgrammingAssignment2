#The following codes and functions are used to create a program that stores and 
#caches a matrix and its inverse. 
#The first function is called makeCacheMatrix and it creates a unique matrix


makeCacheMatrix <- function(x = matrix()) {
    invma <- NULL                                       
  set <- function(y) {
          x <<- y
          invma <<- NULL}
  get <- function() x                                 
  setinverse <- function(inverse) invma <<- inverse
  getinverse <- function() {
                            inver<-ginvma(x)
                            inver%*%x
                            }                         
  list(set = set,
       get = get,
       setinvma = setinvma,
       getinvma = getinvma)}

#The codes and the functions computes the inverse of the specific "matrix" which is created by the first function makeCacheMatrix. 
#The second function is called the cacheSolve and it calculates the inverse of the matrix
#If the inverse of the matrix has been calculated, the function cacheSolve must retrieve it from the cache.


cacheSolve <- function(x, ...) {                
  invma <- x$getinvma()
  if (!is.null(invma)) {                        
          message("loading....getting the data")
          return(invma)}                            
  data <- x$get()
  invma <- solve(data, ...)
  x$setinvma(invma)
  invma}                                        
