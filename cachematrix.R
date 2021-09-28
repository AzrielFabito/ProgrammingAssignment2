#The following codes and functions are used to create a program that stores and 
#caches a matrix and its inverse. 
#The first function is called makeCacheMatrix and it creates a unique matrix


makeCacheMatrix <- function(x = matrix()) {
    invma <- NULL                                       #Initializes the inverse as null
  set <- function(y) {
          x <<- y
          invma <<- NULL}
  get <- function() x                                   #Gets the matrix x
  setinverse <- function(inverse) invma <<- inverse
  getinverse <- function() {
                            inver<-ginvma(x)
                            inver%*%x
                            }                           #Gets the inverse of the matrix
  list(set = set,
       get = get,
       setinvma = setinvma,
       getinvma = getinvma)}

#The codes and the functions computes the inverse of the specific "matrix" which is created by the first function makeCacheMatrix. 
#The second function is called the cacheSolve and it calculates the inverse of the matrix
#If the inverse of the matrix has been calculated, the function cacheSolve must retrieve it from the cache.


cacheSolve <- function(x, ...) {                 #Extracts the cache data 
  invma <- x$getinvma()
  if (!is.null(invma)) {                         #Verifying if the inverse of the matrix is null
          message("getting cached data")
          return(invma)}                         #Returns the value       
  data <- x$get()
  invma <- solve(data, ...)
  x$setinvma(invma)
  invma}                                         #Returns a matrix that which is the invers of the "x"
