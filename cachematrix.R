## Programming Assignment 2 for R Programming on Coursera

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ivs = NULL
  set <- function(y){
    x <<- y
    ivs <<- NULL
  }
  get <- function() x
  setivs <- function(inverse) ivs <<- inverse
  getivs <- function() ivs
  list(set = set, get = get,
       setivs = setivs,
       getivs = getivs)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getivs()
  if (!is.null(inverse)){
    print("Getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data,...)
  x$setivs(inverse)
  inverse
}
