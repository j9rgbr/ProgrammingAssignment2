## This function takes a matrix as an input and will return its inverse.
## The solution will be stored or cached to reduce computational resources.

makeCacheMatrix <- function(x = matrix()) {
  cachedSolution <- NULL
  
  ## Set the value of the matrix and clear the cache
  set <- function(y) {
    x <<- y
    cachedSolution <<- NULL
  }
  
  ## Get the stored matrix
  get <- function() x 
  
  ## Set the value of the solution
  setMatrix <- function(solve) cachedSolution <<- solve
  
  ## Get the stored solution
  getMatrix <- function() cachedSolution
  
  ## Returns functions to the parent environment
  list(set = set, get = get,
       setMatrix = setMatrix,
       getMatrix = getMatrix)
}


## This function will return a matrix that is the inverse of 'x'.

cacheSolve <- function(x, ...) {

  ## Get the stored solution from makeCacheMatrix
  cachedSolution <- x$getMatrix()
  
  ## Skip calculation if the stored solution is not null
  if(!is.null(cachedSolution)) {
    message("getting cached data")
    return(cachedSolution)
  }
  
  ## Get the input matrix if the stored solution does not exist
  data <- x$get()
  
  ## Calculate the inverse of the matrix
  cachedSolution <- solve(data, ...)
  
  ## Store the calculated inverse and return it as the solution
  x$setMatrix(cachedSolution)
  cachedSolution
  
}
