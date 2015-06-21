## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## return: a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##         this list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {  
  
  inv = NULL
  set = function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # which is different from current environment. 
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setInverse = function(inverse) inv <<- inverse 
  getInverse = function() inv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)

}


## Write a short comment describing this function

## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  
  inv = x$getInverse()
  
  # if the inverse has already been calculated
  # get it from the cache and skips the computation.
  if (!is.null(inv)){     
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setInverse function.
  x$setInverse(inv)
  
  return(inv)
}