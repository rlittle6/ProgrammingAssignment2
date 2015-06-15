## The functions create a special "matrix" object that can cache its inverse.
## The inverse of the "matrix" object is calculated or if unchanged, retrieved from the cache. 

## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse 
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Calculates the inverse of the matrix object or if unchanged
## retrieves the inverse from the cache.
## The function is always assumed to be invertible.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Obtaining cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## Example run:
## set working directory to correct location
## >source ("cachematrix.R")
## >x <- rbind(c(1, 2), c(2, 1))
## >m = makeCacheMatrix(x)
## >m$get()
##      [,1] [,2]
## [1,]    1    2
## [2,]    2    1

## >cacheSolve(m)
##            [,1]       [,2]
## [1,] -0.3333333  0.6666667
## [2,]  0.6666667 -0.3333333