## Create a function to create a special matrix object with caching capabilities
## This function returns a list containing two functions: setMatrix and getInverse
makeCacheMatrix <- function(x = matrix()) {
  
  mat <- NULL         # Initialize mat to hold the matrix
  inv_cache <- NULL   # Initialize inv_cache to hold the inverse of the matrix
  
  ## Function to set the matrix
  setMatrix <- function(x) {
    mat <<- x          # Assign the input matrix 'x' to the 'mat' variable using <<- to modify the parent environment
    inv_cache <<- NULL # Clear the inverse cache since the matrix has changed
  }
  
  ## Function to compute and cache the inverse of the matrix
  getInverse <- function() {
    if (is.null(inv_cache)) {
      inv_cache <<- solve(mat)  # Compute the inverse of 'mat' and cache it in inv_cache
    }
    inv_cache   # Return the cached inverse
  }
  
  ## Return a list containing the setMatrix and getInverse functions
  list(setMatrix = setMatrix, getInverse = getInverse)
}

## Function to compute the inverse of a matrix using caching mechanism
## This function retrieves the cached inverse if available, otherwise computes it
cacheSolve <- function(x, ...) {
  inv_cache <- x$getInverse()   # Retrieve the cached inverse using the getInverse method
  
  inv_cache   # Return the cached inverse matrix
}
