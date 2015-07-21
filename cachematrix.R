## These functions create a matrix object, and cache its inverse of that matrix

## This function creates the matrix object, which contains functions such as
## setting and getting the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
	  x <<- y
	  inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set=set, get=get,
	     setinverse=setinverse,
	     getinverse=getinverse)
}

## This function calculates and caches, or simply retrives if it already exists, 
## the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting the cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
