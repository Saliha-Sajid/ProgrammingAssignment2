## The function below creates getters and setters for both the matrix and the inverse.

makeCacheMatrix <- function(x = matrix()) {
  get <- function(){
    return(x)
  }

  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }

  getinverse <- function(){
    return(i)
  }

  setinverse <- function(inverse){
    i <<- inverse
  }

  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The function below checks if the inverse has already been calculated. If it's true, the inverse is returned,
## else the new inverse is calculated.


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  # If the inverse has already been calculated
  i <- x$getinverse()

  if (!is.null(i)) {
    print("getting cached data")
    return(i)
  }

  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  return(i)
}
