## Jeff Carter
## jeffcarter@mac.com
## R Programming (July 25 2015)
## Assignment #2
## https://class.coursera.org/rprog-030/human_grading/view/courses/975104/assessments/3

## These functions cache and compute the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(mtx = matrix()) {
     inverse <- NULL
     set <- function(x) {
          mtx <<- x;
          inverse <<- NULL;
     }
     get <- function() return(mtx);
     setinv <- function(inv) inverse <<- inv;
     getinv <- function() return(inverse);
     return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(mtx, ...) {
     inverse <- mtx$getinv()
     if(!is.null(inverse)) {
          message("Getting cached data...")
          return(inverse)
     }
     data <- mtx$get()
     invserse <- solve(data, ...)
     mtx$setinv(inverse)
     return(inverse)
}