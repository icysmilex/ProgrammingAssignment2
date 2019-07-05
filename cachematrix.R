## Coursera Programming Assignment

##This program was written to cache the inverse of a matrix 



## The following functions are used to store a matrix and cache its inverse.

## makeCacheMatrix takes a matrix and creates a special "matrix" which is really
## a list that:

## 1. Sets the value of the matrix
## 2. Gets the value of the matrix
## 3. Sets the value of the inverse
## 4. Gets the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve computes the inverse of what is returned by the function above.
## If the inverse has already been solved, however, then cacheSolve returns the
## cached inverse instead of computing it again

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i}

Data <- matrix(1:10,2,2)
firstset<-makeCacheMatrix(Data)
firstset$get()

secondset <- cacheSolve(firstset)
secondset
