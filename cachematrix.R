## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

## A function that creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  invMat  <- NULL
  set <- function(y){
    x <<- y
    invMat <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) invMat <<- inverse
  getInverse <- function() invMat   
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

## A function that computes the inverse of the special "matrix" created by makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invMat <- x$getInverse()
  if(!is.null(invMat)){
    message("getting cached data")
    return(invMat)
  }
  mat <- x$get()
  invMat <- solve(mat,...)
  x$setInverse(invMat)
  invMat
}


# Testing the Functions
test_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
test_matrix$get()
test_matrix$getInverse()
cacheSolve(test_matrix)
test_matrix$getInverse()
test_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
test_matrix$get()
test_matrix$getInverse()
cacheSolve(test_matrix)
test_matrix$getInverse()

