## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function creates a special "matrix" object that can cache its inverse
#set value of matrix, get value of matrix
#set inverse matrix, get inverse matrix

#take matrix as input
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { #set value of matrix
    x <<- y #<<- assign value to obj in env that is diff from current env
    m <<- NULL
  }
  get <- function() x #get value of matrix
  setinverse <- function(inverse) m <<- inverse #set inv of matrix
  getinverse <- function() m #get inv of matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function
#Takes output of makeCacheMatrix() as an input
#Checks if matrix has already been cached
#if not already cached, cacheSolve() returns inverse of matrix

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
  
}
#Test Code
#TestMatrix <- matrix(1:4,2,2)
#CacheMatrix <- makeCacheMatrix(TestMatrix)
#CacheMatrix$get()
#CacheMatrix$getinv()
#cacheSolve(CacheMatrix)
