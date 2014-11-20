
## applies the solve() function to a matrix, stores the return the resulting value
## cacheSolve() accesses the object within makeCacheMatrix() and fetches 
## any previously stored values.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


##cacheSolve gets the inverse matrix from x$getsolve()
##if there is a cached matrix x$getsolve() returns a non-Null value then
##matrix (m) is returned and the function ends. Else if Null the original value
##(x$get()) is fetched and solve() is ran on that matrix. The inverse of the 
##matrix is then returned.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m      ## Return a matrix that is the inverse of 'x'
}
