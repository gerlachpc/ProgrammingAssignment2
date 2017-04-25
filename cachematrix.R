## The below functions store a matrix, calculate its inversion,
## store the inversion, and retrieve the inversion if it's already
## been calculated.

### First function
## Creates a vector containing four functions to set and retrieve
## a matrix, and to set and retrieve the inversion.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinversion <- function(inversion) m <<- inversion
  getinversion <- function() m
  list(set = set, get = get,
       setinversion = setinversion,
       getinversion = getinversion)
}

### Second function
## Takes the output from the first function, calls the
## functions contained within, and calculates and stores the 
## matrix inversion using the setinversion function. If the inversion
## has already been calculated, retrieves it using getinversion.

cacheSolve <- function(x, ...) {
  m <- x$getinversion()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinversion(m)
  m
}
