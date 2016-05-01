## Functions below can be used for implementation of cached inverse matrix pattern. 
## Any square matrix can be inversed. However, if the inverse is already performed,
## the result is returned from the cache.

## This function creates a cache model for matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  if(is.na(x) || class(x) != "matrix" || dim(x)[1] != dim(x)[2]){
    print("ERROR Wrong Input - should be of type square matrix")
    return
  }

    
  set <- function(y) {
    if(is.na(y) || class(y) != "matrix" || dim(y)[1] != dim(y)[2]){
      print("ERROR Wrong Input - should be of type square matrix")
      return
    }
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) im <<- inverse
  getinverse <- function() im
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns inverse of a matrix. It returns from cache if value is there
## else it computes and returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  im <- x$getinverse()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setinverse(im)
  im
}
