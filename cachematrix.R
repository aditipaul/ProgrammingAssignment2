## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix: This function will create the "matrix" object which can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse property
  i<-NULL
  
  ## Method to set the matrix
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  ## Method to get the matrix
  get <- function() x
  
  
  ## method to set the inverse of the matrix
  setInverse <- function(inverse) i <<- inverse
  
  ## method to get the inverse of the matrix
  getInverse <- function() i
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

## cacheSolve: This function computes the inverse of the matrix.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  
  if( !is.null(i) ) {
    message("getting cached data")
    return(i)
  }
  ## Get the matrix
  data <- x$get()
  ## Calculate the inverse
  i <- solve(data) %*% data
  ## Set the inverse to the object.
  x$setInverse(i)
  ## Return the matrix
  i      
}