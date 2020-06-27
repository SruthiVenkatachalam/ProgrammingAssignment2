## This program is to cache the inverse of a matrix


##the makeCacheMatrix function is to create a matrix and it's inverse.
  
  makeCacheMatrix <- function(x = matrix()) {
    z <- NULL
    set <- function(y) {
      x <<- y
      z <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) z <<- inverse
    getinverse <- function() z
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }
  
  ## This is to check if the inverse is already existing in the cache 
cacheSolve <- function(x, ...) {
  z <- x$getinverse()
  if (!is.null(z)) {
    message("getting cached data")
    return(z)
  }
  data <- x$get()
  z <- solve(data, ...)
  x$setinverse(z)
  z
}
