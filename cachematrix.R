@@ -1,15 +1,55 @@
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

# the function makeCacheMatrix creates a special matrix object
# that can cache its inverse
# input: matrix m
makeCacheMatrix <- function(m=matrix()){
  im <- NULL # im is inverse of matrix m
  
  # set function to assign different a matrix 
  set <- function(y){
    m <<- y
    im <<- NULL
  }
  
  # function returns matrix m
  get <- function() m
  
  # function store inverse in cache 
  setInverse <- function(inverse) im <<- inverse
  
  # function return inverse 
  getInverse <- function() im
  
  list(set=set, get=get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
# function computes the inversion od the special matrix 
# returned by makeCacheMatrix
# input: x makeCacheMatrix typed function
# eg., makeCacheMatrix(m), where m is matrix we want to
# compute the inverse
cacheSolve <- function(x,...){
  
  im <- x$getInverse()
  if(!is.null(im)){
    message("getting cached data")
    return(im)
  }
  message("inverse not in cached data")
  
  # the inverse of m is not found in cache,
  # so need to cal function solve(m) to compute it
  
  # m: matrix 
  m <- x$get()
  
  # im: inverse of m
  im <- solve(m,...)
  
  # set the inverse im to cache
  x$setInverse(im)
  
  # return inverse mi
  im
}
 No newline at end of file

