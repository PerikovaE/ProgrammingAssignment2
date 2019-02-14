## This function creates a special "matrix" object that can cache its inverse and then cacheSolve calculates the inverse of the matrix.
## If the matrix inverse has already been calculated, it will instead find it in the cache and return it, and not calculate it again.


makeCacheMatrix <- function(x = matrix()) {
  k <- NULL
  set <- function(y){
    x <<- y
    k <<- NULL
  }
get <- function()x
setinverse <- function(invers) k <<- inverse
getinverse <- function() k
list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}


## The function cacheSolve returns the inverse of a matrix created with the makeCacheMatrix function.
## If the cached inverse is available, cacheSolve retrieves it, while if not, it computes, caches, and returns it.

cacheSolve <- function(x, ...) {
  k <- x$getinverse()
  if (!is.null(k)){
    print("getting cached inverse matrix")
    return(k)
  } else {
    k <- solve(x$get())
    x$setinverse(k)
    return(k)
  }
}
