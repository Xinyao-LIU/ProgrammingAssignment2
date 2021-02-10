## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
        x <<- y
        m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      ## Return a list of the methods
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## Get the matrix from our object
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
      ## Calculate the inverse using matrix multiplication
        m <- solve(data, ...)
      ## Set the inverse to the object
        x$setinverse(m)
        m
}
