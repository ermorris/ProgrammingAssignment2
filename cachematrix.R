makeCacheMatrix <- function(x = matrix()) {
  ## Two functions created to help reduce the computation time of large invertible matrix files
  ##
  ##
  ## This function, makeCacheMatrix creates a invertible matrix and
  ## returns a list containing the matrix using functions to
  ##              1. set matrix
  ##              2. get matrix
  ##              3. set inverse
  ##              4. get inverse
  ##         this list is the input to cacheSolve()
  
  invrs = NULL
  set = function(y) {
    # use `<<-` to assign a value to objects in an environment 
    # different from the current. 
    x <<- y
    invrs <<- NULL
  }
  get = function() x
  setinvrs = function(inverse) invrs <<- inverse 
  getinvrs = function() invrs
  list(set=set, get=get, setinvrs=setinvrs, getinvrs=getinvrs)
}

cacheSolve <- function(x, ...) {
  ## x is the output of makeCacheMatrix()
  ## Return a matrix that is the inverse of 'x'
  
  invrs = x$getinvrs()
  
  # if the inverse is already calculated
  if (!is.null(invrs)){
    # it is pulled from the cache and skips the computation. 
    message("pulling cached data")
    return(invrs)
  }
  
  # otherwise, calculates the inverse 
  matrix.data = x$get()
  invrs = solve(matrix.data, ...)
  
  # sets the value of the inverse in cache using the setinvrs function.
  x$setinvrs(invrs)
  
  return(invrs)
}