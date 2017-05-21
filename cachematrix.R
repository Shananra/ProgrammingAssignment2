

## Pseudo class that acts as a getter and setter for matrix cache objects
## Expects the object x to already be solved before sending it here for caching

makeCacheMatrix <- function(origMatrix = matrix()) {
  inverse <- NULL
  set <- function(providedMatrix) {
    origMatrix <<- providedMatrix
    inverse <<- NULL
  }
  get <- function() origMatrix
  setinverse <- function(providedInverse) inverse <<- providedInverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Check if a cached version is already available via makeCacheMatrix
## If available, fetch cached version and skip solve
## If not available, solve and save the cached version.
## Accepts list returned by makeCacheMatrix
## Returns solved matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  resultMatrix <- x$getinverse()
  if(!is.null(resultMatrix)) {
    message("getting cached data")
    return(resultMatrix)
  }
  data <- x$get()
  resultMatrix <- solve(data, ...)
  x$setinverse(resultMatrix)
  resultMatrix
}
