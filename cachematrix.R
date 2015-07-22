## Because some computations can take quite awhile to run, the following
## functions help to reduce the time needed to compute a matrix inversion.
## Given a very large matrix or computing these within loops, the following
## functions compute and then cache an inverse matrix to be called on when 
## needed. This will reduce computational time dramatically.

## This function acutally creates a list containing several functions 
## that do the following: Sets the value of a matrix, Gets the value of 
## a matrix, Sets the value of the inverse matrix, and gets the value
## of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list (set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function first identifies whether the inverse has already been 
## computed. If it has, it will get that inverse from the cache (saving time).
## If it has not, then and only then will it compute and set the inverse to be
## used later. 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setinverse(m)
  m
}
