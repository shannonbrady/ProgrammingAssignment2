## This function allows you to set and get a matrix and to solve for the inverse

## Creates a matrix which is a list of functions allowing you to
# 1. set the value of matrix
# 2. get the value of matrix
# 3. set the value of inverse
# 4. get the value of inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## If m is already set, returns m, if not, calculates inverse of matrix

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting chached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
print("Hello")