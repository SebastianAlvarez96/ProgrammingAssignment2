## This code finds the inverse of a matrix and if the inverse of a matrix has been calculated and cached it
## it wouldn't calculated again

makeCacheMatrix <- function(x = matrix()) {
      ## Creates a "special" matrix and this function set and get the value of the matrix and set and get 
      ## the inverse of the matrix
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function() inv
      list(set = set, get = get,
          setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
      ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
      ##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
      ##should retrieve the inverse from the cache
   
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getInverse()
      if (!is.null(inv)) {
            message("getting cached data")
            return (inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setInverse(inv)
      inv
}
