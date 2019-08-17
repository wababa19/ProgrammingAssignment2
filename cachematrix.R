## Function that creates and cache the inverse matrix


## The function load the package matrixcalc, which is collection
## of functions for matrix calculations. We need the functiom 
## matrix.inverse in order to calculate inverse of matrix
## Before computations, the function check the singularity 
## of matrix

makeCacheMatrix <- function(x = matrix()) {
  require(matrixcalc)
  # check if the matrix is singular, i.e. irreversible
  if(is.singular.matrix(x) == TRUE){
    stop('Matrix in irreversible !')}
  minv<- NULL
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) minv <<- inverse
  getinverse <- function() minv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns inverse matrix

cacheSolve<- function(x, ...) {
  minv <- x$getinverse()
  if(!is.null(minv)) {
    message("getting cached inverse matrix")
    return(minv)
  }
  data <- x$get()
  minv <- solve(data, ...)
  x$setinverse(minv)
  minv
}
