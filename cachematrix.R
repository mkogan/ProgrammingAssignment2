## makeCacheMatrix takes matrix as input and outputs a list with four functions, these functions
## make use of variables in the parent environment for caching.
## cacheSolve takes the cached variables from makeCacheMatrix and either retrieves the previously calculated
## inverse or else calculates it.


## Create cached variables
makeCacheMatrix <- function(x = matrix()) {
  ## set matrix inverse to null
  m <- NULL
  ## if new matrix is set, then assign this value to x variable in parent environment and reset inverse to null 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## retrieves matrix data
  get <- function() x
  ## sets the cached inverse variable
  setinverse <- function(solve) m <<- solve
  ## retrieves cached inverse variable (since m is a free variable it will be retrieved from parent environment)
  getinverse <- function() m
  #return a list of 4 functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculate matrix inverse, or else retrieve cached variables

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  ## if m is null, inverse of x was never calculated or the matrix has been changed
  ## if m is not null we will retrieve the value of m
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## retrieve matrix, calculate inverse, and set cached variable to inverse value
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
