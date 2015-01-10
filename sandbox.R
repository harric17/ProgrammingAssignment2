makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}




z=1:10

xx <- makeVector(z)
xx$get()
xx$setmean(mean(z))
xx$getmean()

cachemean(xx)






xx <- makeCacheMatrix(matrix(c(1,2,3,0,1,4,5,6,0),nrow=3,ncol=3))
xx$get()
xx$getinv()

cacheSolve(xx)




