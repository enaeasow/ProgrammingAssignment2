
# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse<- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data") # Display message whenever same output is called
    return(m)                      # return inversed matrix stored as m     
  }
  data <- x$get()  
  m <- solve(data, ...)  # Function to get inverse of square matrix
  x$setinverse(m)  # Set the inverted output matrix
  m
}

