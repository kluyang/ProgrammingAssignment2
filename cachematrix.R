## Programming Assignment 2 for R Programming on Coursera

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ivs = NULL
  set <- function(y){
    x <<- y
    ivs <<- NULL
  }
  get <- function() x
  setivs <- function(i) ivs <<- i
  getivs <- function() ivs
  list(set = set, get = get,
       setivs = setivs,
       getivs = getivs)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  xcheck <- x$get()
  if (xcheck = x)
    i <- getivs()
    if (!is.null(i)){
      message("Getting cached data")
      return(i)
    }
  data <- x$get()
  i <- solve(data)
  i
}
