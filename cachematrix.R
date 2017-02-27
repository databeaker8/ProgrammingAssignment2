## These functions solve for the inverse of an input matrix.

## makeCacheMatrix creates a list of four functions.  These functions 
## initialize and define the input matrix and cache the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve takes the makeCacheMatrix object as an input
## it checks if the inverse of the matrix has been cached.  
## If it has, it retrieves that value.  If it hasn't, it calculates
## the inverse and caches the result.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    print("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


