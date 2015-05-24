## Function 'makeCachematrix' creates a special "matrix" with 
## following functions:
## 
## set - set the value of the matrix
## get - get the value of the matrix
## setinv - set the inverse of the matrix
## getinv - get the invers of the matrix

makeCacheMatrix <- function(x = matrix()) {
  invertedMatrix <- NULL
  set <- function(y) {
      x <<- y
      invertedMatrix <<- NULL
    }
  
  get <- function() {
    x
  }
  
  setinv <- function(solvedMatrix) {
    invertedMatrix <<- solvedMatrix
  }
  
  getinv <- function() {
    invertedMatrix
  }
  
  list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Function cacheSolve takes an argument of type 'makeCacheMatrix'
## and returns the inverse of the matrix. If inverse is stored then 
## the stored value is returned, otherwise inverse will be calculated.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()   ## retrieves the inverse matrix value
  
  if(is.null(inv)) { ## check if the retrieved value is null
    data <- x$get()   ## retrieve the matrix to be inverted
    inv <- solve(data, ...) ## calculate the inverse 
    x$setinv(inv)     ## store the inverse value
  }
  
  else {
    message("getting cached data") ## print text to console
  }
  
  return(inv) ## returns the inverse value
}
