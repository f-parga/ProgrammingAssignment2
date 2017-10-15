# --------------------------------------------
# F.Parga - 15/10/2017
# Coursera - R Programming - Week 3
# Lexical Scoping Assigment
# --------------------------------------------

# This script will compute the inverse of a matrix and then cache it for future use (instead of recalculating it).

# -------------
# Example Usage:
# -------------

# testMatrix <- makeCacheMatrix(matrix(1:4,2,2))  // Create the testMatrix object and set a 2x2 matrix. The function can be declared without a matrix argument.

# testMatrix$setMatrix(matrix(1:4,2,2))   //Sets a matrix value and overrides any exisiting, previous value.

# testMatrix$getMatrix()   //Returns the currently stored matrix

# testMatrix$getInverse()  //Returns the cached inverse value - If the inverse has not been calculated yet, it returns NULL.

# cacheSolve(testMatrix)   //Solves for the inverse of the matrix and then assigns it to the cache. If the inverse ahs already

# been cached, it will return the cached value with some text to let you know it is using a cached value.


makeCacheMatrix <- function(x = numeric()) {
  #create a null cache value
  tempcache <- NULL
  
  # store a matrix
  setMatrix <- function(newMatrix) {
    x <<- newMatrix
    # the cached value should be cleared on the assingment of a new matrix (the pre-computed inverse corresponds to an different matrix).
    tempcache <<- NULL
  }
  
  # retrieve the stored matrix
  getMatrix <- function() x
  
  # work out and store the given argument 
  setInverse <- function(solve) {
    tempcache <<- solve
  }
  
  # get the cached value
  getInverse <- function() tempcache
  
  # return a list of functions
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


# The following function calculates the inverse of a matrix created with makeCacheMatrix
cacheSolve <- function(x, ...) {
  
  # check if a cached value exists - if it does, return it
  if(!is.null(x$getInverse())) {
    inverse <- x$getInverse()
    message("Returning cached data - The inverse has not been re-computed.")
    return(inverse)
  }
  
  # if the cached value does not exist, calculate the inverse of the matrix and store it.
  inputMatrix <- x$getMatrix()
  inverse <- solve(inputMatrix)
  x$setInverse(inverse)
  
  inverse #return the inverse
}