##Intialize the Cached inverted matrix
## Set the object's matrix
##get the object matrix
## Set the inverted matrix into cache by using cacheSolve()
##Get the inverted matrix from the cache
## List of the functions included into makeCacheMatrix() objects



makeCacheMatrix <- function(x = matrix()) {
  # Initialize the cached inverted matrix
  inverse <- NULL 
  # Set the object's matrix
  set <- function(y) { 
    x <<- y
    inverse <<- NULL
  }
  # Get the object's matrix
  get <- function() x  
  # Set the inverted matrix into cache by using CacheSolve()
  setCacheMatrix <- function(solve) inverse <<- solve 
  # Get the inverted matrix from the cache.
  getCacheMatrix <- function() inverse 
  # List of the functions included into makeCacheMatrix() objects
  list(set=set, get=get, setCacheMatrix=setCacheMatrix, 
       getCacheMatrix=getCacheMatrix)
}







## cacheSolve() solves the matrix object created with the makeCacheMatrix()
## function. It first test if there already is a cached inverted matrix of the
## object (getCacheMatrix() from the makeCacheMatrix() function) instead of
## recalculate it. If there isn't, then the inverted matrix is written 
## with setCacheMatrix().




cacheSolve <- function(x, ...) {
  # Check if there is a cached inverted matrix
  inverse <- x$getCacheMatrix()
  if(!is.null(inverse)) {
    # If there is, then print a message and return it
    message("There is already inverted matrix")
    return(inverse)
  }
  # If there is not, get the original matrix, inverse it, set it into
  # cache and return the inverted matrix.
  inputmatrix <- x$get()
  inverse <- solve(inputmatrix, ...) 
  x$setCacheMatrix(inverse)
  inverse
}


