## This program provides the inverse of any given matrix (assuming it's always invertible) 
## with caching mechanism to avoid excessive use of machine computing resources.

## Function to make the special matrix and setting-up other parameters
makeCacheMatrix <- function(x = numeric(),y = numeric(),z = numeric()){
  x <- rbind(x,y,z)
  m <- NULL
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Function to solve the inverse of a matrix and caching its output.
cacheSolve <- function(A, ...) {
  m <- A$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- A$get()
  m <- solve(data, ...)
  A$setInverse(m)
  m
}

## calling the two functions
c <- makeCacheMatrix(x <- c(3,2,5),y <- c(2,3,2),z <- c(5,2,4))
cacheSolve(c)
cacheSolve(c)
