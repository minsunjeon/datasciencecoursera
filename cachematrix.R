##Matrix inversion is usually a costly computation and there may be some benefit 
##to caching the inverse of a matrix rather than compute it repeatedly.
##The following pair of functions cache the inverse of a matrix.


##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = numeric()) {  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getmean <- function() i
  list(set = set, get = get,
       setmean = setinverse,
       getmean = getinverse)
}



## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
      

