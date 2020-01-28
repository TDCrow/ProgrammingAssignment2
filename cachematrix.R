## These functions allow the user to set and generate a matrix and its inverse. Once calcualated
## the matrix and its inverse can be called without the need for further calculation

## makeCacheMatrix creates a list of 4 functions: 1. allows you to set a matrix different to
## that which is previously saved, 2. allows you to get that matrix, 3. allows you to 
## set an inverse of the matrix, 4. allows you to get that inverse

makeCacheMatrix <- function(x = matrix()) {

    inverse_m <- NULL
  set <- function(y = matrix()) {
    x <<- y
    inverse_m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse_m <<- inverse
  getinverse <- function() inverse_m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## cacheSolve returns the inverse of a matrix 'x'. If an inverse of this matrix
## has already been generated it returns "getting cached data" before showing the
## matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Return a matrix that is the inverse of 'x'
  inverse_m <- x$getinverse()
  if(!is.null(inverse_m)) {
    message("getting cached data")
    return(inverse_m)
  }
  else {
    matrix_to_transform <- x$get()
    inverse_m <- solve(matrix_to_transform)
    x$setinverse(inverse_m)
    inverse_m
  }
  
}
