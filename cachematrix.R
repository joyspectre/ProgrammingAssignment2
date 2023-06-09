## June 8, 2023; Course 2 Programming Assignment 2 (Lexical Scoping)
## Submitted by JoyC
############################
## functions makeCacheMatrix and cacheSolve 1. stores a matrix supplied by the user
## 2. Creates a list containing four functions 3. Computes the inverse of a matrix
## and stores it in a cache, which can be recalled if the user re-runs cacheSolve
## with the same matrix.

############################
## makeCacheMatrix takes the argument (matrix(matrix, number of rows, number of columns))
## it stores the matrix, along with a list of four functions to set and get the 
## matrix and its inverse.
## If user supplied value that is not a matrix, makeCacheMatrix will return
## a message asking for a matrix
## e.g. matrix1 <- makeCacheMatrix(matrix(matrix, row, col))
## calling matrix1 prints a list with four functions (set, get, setolve, getsolve)
## matrix1$set(new matrix) will change the matrix without needing to re-run
## makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
  
  not_matrix <- "User supplied argument is not a matrix. Please supply a matrix"
  
  if (is.matrix(x)) {
    m <- NULL
    set <- function(y) {
      if (is.matrix(y)) {
        x <<- y
        m <<- NULL
      }
      else message(not_matrix)
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
    
  } else message(not_matrix)
}


################################
## cacheSolve takes the argument cacheSolve(object created with makeCacheMatrix)
## cacheSolve computes and returns the inverse of the matrix supplied in 
## makeCacheMatrix
## If the matrix inverse was previously computed, cacheSolve will return the 
## cached value.
## e.g. matrix1_inv <- cacheSolve(matrix1)
## returns a matrix that is the inverse of matrix1
## supplying a non-invertible matrix will give an error


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
     message("getting cached data")
    return(m)
    }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}


