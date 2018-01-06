## Put comments here that give an overall description of what your
## functions do


## Function that can catche it's inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	 inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## computing the inverse that was returned by makeCacheMatrix.
##inverse has been already calculated then this dunction will use the
##catched inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
 	   if (!is.null(inv)) {
 	     message("getting cached data")
 	     return(inv)
 	   }
 	   mat <- x$get()
 	   inv <- solve(mat, ...)
 	   x$setInverse(inv)
 	   inv
}


