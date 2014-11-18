## Programming Assignment 2 -- Lexical Scoping.
##
## A pair of functions that cache the inverse of a matrix

## The function makeCacheMatrix create the matrix, cache its inverse
makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     ## set the matrix
     set <- function(y) {
            x <<- y
            m <<- NULL
     }

     ## get the matrix
     get <- function() x
     setmatrix <- function(solve) m <<- solve
     getmatrix <- function() m
     list(set = set, get = get,
           setmatrix = setmatrix,
           getmatrix = getmatrix)
}

## The function cacheSolve absorbs all arguments from makeCacheMatrix, 
## check if the inverse has been calculated or not.
## This function assume that the matrix is always invertible.
## Calculate the inverse or get it from the cache if the matrix has not changed.


cacheSolve <- function(x = matrix(), ...) {
     m <- x$getmatrix()
     ## if the inverse has been calculated, just return the matrix m 
     if(!is.null(m)) {
             message("getting cached data")
             return(m)
      }
 
      data <- x$get()
      ## get the inverse using the solve function
      m <- solve(data, ...)
      x$setmatrix(m)
      m
      ## Return the matrix m that is the inverse of 'x'
}
