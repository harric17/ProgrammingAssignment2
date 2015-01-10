# The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

#Here's an example of how to run it:
# Save matrix to an object named xx:
#       xx <- makeCacheMatrix(matrix(1:4,nrow=2,ncol=2))
# Return your matrix:
#       xx$get()
# Return the inverse matrix:
#       xx$getinv()

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}




## The following function calculates the mean of the special "matrix" created with the above function. 
## However, it first checks to see if the mean has already been calculated. 
## If so, it gets the mean from the cache and skips the computation. 
## Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.
##
## Here's an example of how to run it with makeCacheMatrix():
## Save matrix to an object named xx:
##       xx <- makeCacheMatrix(matrix(1:4,nrow=2,ncol=2))
## Run cacheSolve():
##       cacheSolve(xx)


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}



