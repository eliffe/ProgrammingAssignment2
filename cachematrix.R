
## This function creates a matrix object and it has some functions like set and get. 
## This function keep inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inver) inverse <<- inver
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function use x matrix, created by above function, as an argument then it computes inverse of it using solve function
## Solve() function computes the inverse of a matrix.
## If the inverse has been calculated, then it shows the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
