## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function - we hold the cached value (initially nothing is cached), store a matrix and return it, cache the argument, get the cahced value and then return a list. Each element in the list is a function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function - we get the cahced value (if one exists return it), otherwise get the matrix, calculate the inverse and store it in the cache. Return the inverse

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
