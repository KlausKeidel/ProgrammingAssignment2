## Put comments here that give an overall description of what your
## functions do

## The first function makeCacheMatrix creates a speacial "matrix", with is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inversion
## 4. get the value of the matrix inversion


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
          x <<- y
          m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get =get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## This function computes the inverse of the special "matrix"
## 1. get the matrix form "makeCacheMatrix"
## 2. check if the inverse is already calculated and the matrix has not changed, retrieve the inverse from the cache.
## 3. The matrix is new/ has changed, get matrix
## 4. calculate inverse.
## 5. set inverse of the matrix
## 6. retrieve the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getmatrix()
      if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
      data <- x$get()
      m <- solve(data, ...)
      x$setmatrix(m)
      m
}
