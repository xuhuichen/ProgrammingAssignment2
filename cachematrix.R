## Put comments here that give an overall description of what your
## functions do
## These functions create a matrix object, and cache its inverse of that matrix

## Write a short comment describing this function
## This function creates the matrix object

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


## Write a short comment describing this function
## This function calculates and caches, or simply retrive if it exists, 
## the inverse of the matrix

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
