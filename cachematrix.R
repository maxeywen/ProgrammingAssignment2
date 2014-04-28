## 2 functions that will accept a matrix, calculate its inverse, cache the result,
## and return the cached inverse unless the original matrix is changed

## makeCacheMatrix()
## exposes 4 methods in a list
## set, get, setInverse, getInverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inv) i <<- inv
    
    getInverse <- function() i
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## checks cache first for inverse.  if cache is NULL,
## solves the matrix and stores in cache

cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
  
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data)
    x$setInverse(i)
    i
}
