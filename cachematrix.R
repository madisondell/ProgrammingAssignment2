## Together, these two functions generate a square invertible matrix and save the inverted matrix
## to the cache environment

## makeCacheMatrix returns a list of the functions used to get or set the inverted matrix in cache

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  get <- function() x
  setMatrix <- function(inverse) cache <<- inverse
  getInverse <- function() cache
  list(set = set, get = get,
       setMatrix = setMatrix,
       getInverse = getInverse)
}


## cacheSolve calculates the inverse of the matrix generated in makeCacheMatrix
## If the inverted matrix is not available in the cache environment, it is created in the working
## environment and the resulting inverted matrix is saved in cache

cacheSolve <- function(x, ...) {
  cache <- x$getInverse()
  if(!is.null(cache)) {
    message("getting cached data")
    return(cache)
  }
  matrix <- x$get()
  tryCatch( {
    cache <- solve(matrix, ...)
  },
  error = function(e) {
    message("Error:")
    message(e)
    
    return(NA)
  },
  warning = function(e) {
    message("Warning:")
    message(e)
    
    return(NA)
  },
  finally = {
    x$setMatrix(cache)
  })
  return(cache)
}
