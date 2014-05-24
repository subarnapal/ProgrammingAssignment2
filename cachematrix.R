## This function creates a special "matrix" object that can cache 
## its inverse
makeCacheMatrix <- function(a = matrix(), rows, cols){
  if (!rows == cols)
    stop("The matrix provided doesn't have an inverse")
  x <- matrix(a, rows, cols)
  i <- NULL
  set <- function(y, rows, cols) {
    if (!rows == cols)
      stop("The matrix provided doesn't have an inverse")
    x <<- matrix(y, rows, cols)
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix
cacheSolve <- function(x){
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}