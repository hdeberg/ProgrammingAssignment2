## Put comments here that give an overall description of what your
## functions do

##These functions compute the inverse of a matrix. To minimize computation
##time, they do this by creating a special object (a list of four functions)
##that can be used to cache whether the inverse has been computed before.
##If the inverse has been created before, the result of the previous computation
##is looked up and returned. If not, the inverse is computed and returned.

## Write a short comment describing this function


## makeCacheMatrix creates a special "matrix", which is really a list containing 
## four function to 
##  1.set the value of the matrix
##  2.get the value of the matrix
##  3.set the value of the inverse
##  4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Write a short comment describing this function

##cacheSolve checks to see whether the inverse of the matrix has previously
##been computed. If so, it returns the previously computed inverse. If not,
##it computes the inverse (using the solve function) and returns this
##If the matrix is singular and cannot be inverted, a message stating this is returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  
  ##Check if data is singular
  determinant <-det(data)
  if (determinant == 0){
    message("Matrix is singular and canntot be inverted")
    return(invisible())
  }
  
  ##If data is not singular, compute the inverse
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  
}
