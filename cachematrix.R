## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
#Inverse property initialization
  inv <- NULL
  
#Setting the function
  set <- function( matrix ) {
    x <<- matrix
    inv <<- NULL
  }
  
#Get and return the matrix
  
  get <- function() {
    x
  }

#Set inverse
  
  setInverse <- function(inverse) {
    inv <<- inverse
  }

#Get inverse
  
  getInverse <- function() {
    ## Return the inverse property
    inv
  }

#Create list of the methods used
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
        ## Return inverse if it's already set
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
    ## Generate matrix from object
  
  mat <- x$get()
  
    ## Use matrix multiplication to get inverse
  
  x <- solve(mat) %*% mat
  
    ## Set inverse to object  
  x$setInverse(inv)
  inv
}
  

