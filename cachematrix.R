## These two functions are used in combination to cache potentially
## computation-heavy solutions. The first function creates a vector
## that stores the get/set of the matrix as well as a get/set for the inverse
## in cache memory

## This function creates the vector to store the matrix and sets the inverse as
## as NULL since it hasn't been solved yet

makeCacheMatrix <- function(x = numeric()) {
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


## This function first determines if the vector passed has an inverse of a matrix.
## If it doesn't, it then computes the inverse and stores it in the vector as well as
## returning the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
