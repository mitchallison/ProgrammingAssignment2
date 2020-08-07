## Two functions are presented below that will create a cache for a matrix, 
## while the second will, 1) check the cache for a inverted matrix and return if
## if present, 2) if there is no inverted matrix, computer and cache the 
## inverted matrix.


## The first function "makeCacheMatrix will" create a special "matrix" to cache 
## the inverse of that matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL                 ##set m to null
    set <- function(y) {      ##set is a function of y
    y<- matrix()              ##initialize y as a matrix
    x <<- y                   
    m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,   ##create a list to retrieve functions
         setinv = setinv,
         getinv = getinv)
  }



## The second function cacheSolve will first look for the cached inverse of the 
## matrix, and if there isn't one, it will compute the inverse of the matrix
## and cache the result

cacheSolve <- function(x, ...) {
    m <- x$getinv()           ##check m to see if there is an inverse matrix
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()           ## get data from "x" and inverse the matrix
    m <- solve(data, ...)     
    x$setinv(m)               
    m
  }
