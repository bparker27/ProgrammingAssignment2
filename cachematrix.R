## These functions accept a square invertible matrix, cache the value of the matrix,
## then solve for the inverse of the cached matrix and cache the inverted value.
## The test the difference in speed of returning calculated values vs cached values.

## This function caches the value of a matrix
makeCacheMatrix <- function(x = matrix()) {
  
  ## Create local inverse variable with NULL value
  inverse_matrix <- NULL
  
  ## Cache the supplied matrix value and NULL inverse matrix to global
  setMatrix <- function(y) {
    x <<- y
    inverse_matrix <<- NULL
  }
  
  ## Return the cached value of the matrix
  getMatrix <- function() {
    return(x)
  }
  
  ## Assign the solved inverse matrix to the cached inverse matrix variable
  setInverse <- function(inverse) { 
    inverse_matrix <<- inverse
  }
  
  ## Return the cached value of the inverse matrix
  getInverse <- function() {
    return(inverse_matrix)
  }
  
  ## Create special vector for the functions to be passed as arguments to ...
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function solves for the inverse of the cached matrix
cacheSolve <- function(x, ...) {
  
  ## Create local variable and assign the current value of the cached inverse matrix
  inverse_matrix <- x$getInverse()
  
  ## Check if the inverse value has already been calculated, if so return that value
  if(!is.null(inverse_matrix)) {
    message("Cached Data:")
    return(inverse_matrix)
  }
  
  ## If the cached inverse matrix is NULL, get the starting matrix 
  ## and assign it to a local variable
  starting_matrix <- x$getMatrix()
  
  ## Calculate the inverse matrix using the starting matrix local variable
  ## and assign the result to the local inverse matrix variable
  inverse_matrix <- solve(starting_matrix, ...)
  
  ## Assign the calculated inverse matrix to the global inverse matrix
  x$setInverse(inverse_matrix)
  message("Calculated Data:")
  return(inverse_matrix)
}

## This function tests the execution time of caclculating the inverse matrix and
## the execution time of getting the cached inverse matrix
testRun <- function(z) {
  
  ## Create local variable to store the cached matrix value
  cached_matrix <- makeCacheMatrix(z)
  
  ## Create local variable for system start time
  start_time <- Sys.time()
  
  ## Call cache solve on local variable to calculate inverse
  cacheSolve(cached_matrix)
  
  ## Create local variable for system end time
  end_time <- Sys.time()
  
  ## Calculate and print execution time
  execution_time <- end_time - start_time
  print(execution_time)
  
  ## Repeat to see execution time for returning inverse value from cache without calculating
  start_time <- Sys.time()
  cacheSolve(cached_matrix)
  end_time <- Sys.time()
  execution_time <- end_time - start_time
  print(execution_time)
}

## Create a test matrix
test_matrix <- matrix(rnorm(1000000), nrow = 1000, ncol = 1000)

## Call test run on the test matrix
testRun(test_matrix)
