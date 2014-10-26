## Assignment 2: Lexical Scoping
## Objective: To develop a pair of functions that cache the inverse of a matrix
## Functions: 
## 1. makeCacheMatrix = function that creates a special matrix object that can cache its inverse
## 2. cacheSolve = function that computes inverse of the matrix returned by "makeCacheMatrix" function above

## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
  
  ## Initialize the inverse property (inverse) with NULL
  inv_prop <- NULL
  
  ## Use a method to SET THE INITIAL MATRIX
  set <- function( matrix ) {
    m <<- matrix
    inv_prop <<- NULL
  }
  
  ## Use a method to GET THE MATRIX
  get <- function() {
    ## Return matrix m
    m
  }
  
  ## Use a method to SET THE INVERSE OF THE MATRIX
  setInverse <- function(inverse) {
    ## Set inverse property as the inverse provided in set method
    inv_prop <<- inverse
  }
  
  ## Use a method to GET THE INVERSE OF THE MATRIX
  getInverse <- function() {
    ## Return the inverse property
    inv_prop
  }
  
  ## Return all methods in list format
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Compute the inverse of the output matrix returned by "makeCacheMatrix" function above
## If the inverse has already been calculated, use "cachesolve" to retrieve the inverse from the cache.
cacheSolve <- function(input, ...) {
  
  ## Return a matrix that is the inverse of 'input' object
  m <- input$getInverse()
  
  ## Return the inverse if its already set; otherwise if null, skip.
  if( !is.null(m) ) {
    message("Cached data still being retrieved. Waiting...")
    return(m)
  }
  
  ## Get the matrix from 'input'
  data <- input$get()
  
  ## Calculate the inverse by using matrix multiplication
  m <- solve(data) %*% data
  
  ## Set the inverse to the 'input' object
  input$setInverse(m)
  
  ## Return the matrix
  m
}
