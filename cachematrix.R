## These functions allow us to conserve computational resources by 
## using the lexical scoping rules of R to preserve an object's state.

## In summary we: 
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list (set = set, 
        get = get, 
        setsolve = setsolve, 
        getsolve = getsolve)
}


## In this function we are computing the inverse of the matrix above. 
## If the matrix has already been 'solved' and has not changed, 
## this function will get it from the cache.

cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getsolve()
  
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
  
  }
