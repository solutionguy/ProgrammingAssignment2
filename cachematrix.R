## Put comments here that give an overall description of what your
## functions do

# Included the Matrix library to allow the solve function for the Matrices to be available
library(Matrix)

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  invm <- NULL
  
  # set value of the matrix (The setter function, persists the original matrix into environment 'x' )
  set <- function(y) 
  {
    x <<- y    # Persist into environment 'x'
    
    invm <<- NULL # matrix invalidated, reassign NULL into it
  }
  
  # Get value of matrix (The original matrix)
  get <- function() x
  
  
  # Set inverse of matrix (The computed inverse of matrix x)
  setinverse <- function(inverse) invm <<- inverse
  
  
  # Get inverse of matrix (Return the value of invm)
  getinverse <- function() invm
  
  # Return a list containing all functions defined above (The getter & setter methods)
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  # Inverse getter method
  inv <- x$getinverse()
  
  # if inverse object is already there, check if already cached
  # if yes, return cached inverse
  if(!is.null(invm)) {
    message("Getting the cached Matrix...")
    return(invm)
  }
  
  # Else Get otriginal matrix 
  data <- x$get()
  
  # Compute inverse of the above matrix
  invm <- solve(data, ...)
  
  # cache inverse of matrix
  x$setinverse(invm)
  
  # return inverse
  invm

}

