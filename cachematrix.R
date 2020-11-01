## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# the function makeCacheMatrix takes a matrix as a parameter
# and set it to x 
# > m <- makeCacheMatrix(matrix(c(4,2,7,6),ncol = 2))
# > m$get()
#      [,1] [,2]
# [1,]    4    7
# [2,]    2    6
# > m$getinv()
# NULL

makeCacheMatrix <- function(x = matrix()) {
  # inv holds the inverse value
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invers) inv <<- invers
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Write a short comment describing this function
# the function cacheSolve takes an object of makeCacheMatrix as a parameter
# and return the inverse of the matrix that was passed to makeCacheMatrix
# >cacheSolve(m)
#     [,1] [,2]
#[1,]  0.6 -0.7
#[2,] -0.2  0.4 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}