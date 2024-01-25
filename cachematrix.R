## I created a matrix function that cache the value of inverse matrix 
makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setInverse <- function(matrix) inver <<- inverse
  getInverse <- function() inver
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## this is the second function

cacheSolve <- function(x, ...) {
  inver <- x$getInverse()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data, ...)
  x$setInverse(inver)
  inver
}
