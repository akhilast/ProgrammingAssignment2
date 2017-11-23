## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##this function is about caching the inverse of matrix, so this will create
## a speacial obkects which stores a atrix and caches its inverse.
makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set<-function(y){
  x<<-y
  inv <<- NULL
}
get<-function()x
setInverse<-function(inverse)
  inv<<-inverse
getInverse <- function() inv
list(set = set,
     get = get,
     setInverse = setInverse,
     getInverse = getInverse)
}


## Write a short comment describing this function
## This function calculates the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
