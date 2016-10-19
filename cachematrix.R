## Caching the inverse of a matrix
## This function  will create a special matrix object that can cache its inverse
## It is basically an object which contains a list containing functions for further use with the matrix as input
## To use it --> E.g. If you have a 2x2 matrix like this one: matrix_thing <- matrix(c(4,2,5,3),2,2)
## NewMatrixObject <- makeCacheMatrix(matrix_thing)  


makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  get <- function() x
  setxinv <- function(inverse) xinv <<- inverse
  getxinv <- function() xinv
  list(set = set, get = get, setxinv = setxinv, getxinv=getxinv)
}

## This function looks up whether there is an inverse of the input (a matrix) in cache and returns the inverse of 'x'
## If it's not it cache it will get the data from the input and starts performing the function needed to calculate the inverse
## To use --> E.g. you take the NewMatrixObject from the previous function as input: cacheSolve(NewMatrixObject)
## This will look up if there is already an inverse inside the object via the x$getxinv() and if so, it returns the cached inverse
## Otherwise it will retrieve the data with the $get() part and start calculating the inverse with the solve()-function

cacheSolve <- function(x, ...) {
  xinv <- x$getxinv()
  if(!is.null(xinv)) {
    message("obtaining cached data")
    return(xinv)
  }
  data <- x$get()
  xinv <- solve(data, ...)
  x$setxinv(xinv)
}
