## Instead of of solving the inverse of a matrix everytime it is needed
## these functions allow the inverse to be cached and retrieved wihout 
## needing to solve it.

## A function which stores a list of the matrix functions
## set(x) to set the matrix to x
## get() to return a copy of the matrix
## setinverse(x) to set the cached inverse of the matrix to x
## getinverse() to return the cached inverse of the matrix 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, 
       setinverse = setinverse, getinverse = getinverse)

}


## A function which will return the inverse of the matrix
## If the inverse is availble in cached it is returned
## If the inverse isn't availble it is calculated and stored

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) {
    ## the cached inverse exists return it. 
    message("retrieving cached inverse")
    return(m)
  }
  ## the cached inverse doesn't exist
  data <- x$get()
  m <- solve(data, ...)  ## calculate inverse
  x$setinverse(m)  ## cache the inverse
  m
}
