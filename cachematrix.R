## Put comments here that give an overall description of what your
## functions do

## This function make a cache matrix with 4 functions: get, set,
## setInverse, and getInverse.

makeCacheMatrix <- function(x = matrix()) {
  inverseM <- NULL
  
  ## set function will change the cache matrix stored in the main function.
  ## substitute matrix x with y (the input) and restore inverse matrix to NULL
  set <- function(y){
    x <<- y
    inverseM <<- NULL
  }
  
  ## get funcion will return the stored cache matrix
  get <- function() x
  
  ## setInverse function will set the input inverse matrix inv to variable
  ## inverseM
  setInverse <- function(inv) inverseM <<- inv
  
  ## getInverse function will return the inverse matrix stored in main function
  getInverse <- function () inverseM
  
  ## function list() is used to store the above 4 functions in makeCacheMatrix
  list(set=set, get=get, setInverse = setInverse, getInverse = getInverse)
  
}


##The following function calculates the inverse of the cache matrix created
##with the above function. However, it first checks to see if the inverse matrix has 
##already been calculated. If so, it gets the inverse matrix from the cache and skips 
##the computation. Otherwise, it calculates the inverse of the data and sets 
##the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseM <- x$getInverse()
  if (!is.null(inverseM)){
    message ("getting cache inverse matrix")
    return (inverseM)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
  
}

