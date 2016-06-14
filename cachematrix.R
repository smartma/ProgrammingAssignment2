## Mike Smart - R Programming - Assignment 02 - 14/06/2016
##
## The functions below allow for a matrix to be defined and if need be calculate it's inverse.
## If the inverse caluclation is again required the value that is cached is returned rather than
## performing the computation again.

## makeCacheMatrix creates a special "matrix" which is really a lists containing a function to
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse of the matrix
## 4. Get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(solve) inv <<- solve
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve calculates the inverse of the "matrix" created with makeCacheMatrix.
## However it first checks to see if the inverse has already been calculated. If so,
## the cached value is returned, otherwise it computes the inverse and returns it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}
