## CDS 2-15-15 Creates list of 4 Function to be used in the cacheSolve function 
##that gives results of inverse Matrix

## Assign values to golbal varibles x, m

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


##CDS 2-15-15 Finds inverse of Matrix.  1st checks if inverse of matrix has 
## been calculated if so returns result from varaible otherwise calculates inverse of Matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m    
}
