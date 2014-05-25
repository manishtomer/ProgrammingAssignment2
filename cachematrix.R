## This program  create a matrix and then cache its inverse. It has two functions - 1. makeCacheMatrix and 2. cacheSolve
## makeCacheMatrix- This function creates a special "matrix" object that can cache its inverse. 
## cachesolve - This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


## makeCacheMatrix does following
##1. set the value of the matrix
##2. get the value of the matrix
##3. calculate the inverse of the matrix
##4. get the inverse of the matrix

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


## cacheSolve calculates the inverse of the special "matrix" created with the above function. However, it first checks to see if the inverse 
## has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of 
## the matrix and sets the value of the inverse in the cache via the setinverse function.

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
