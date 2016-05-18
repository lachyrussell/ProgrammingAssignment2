#Week 3 Assignment 

#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL #initialise inverse
  
  #overwrite existing matrix?
  #e.g. 
  #my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
  #my_matrix$set(matrix(4:7, 2, 2))
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL #initialise inverse again
  }

    get <- function() x
  
  
  setinverse <- function(inv) {inverse <<- inv
  }
  
  getinverse <- function() inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix
#above. If the inverse has already been calculated (and the matrix has not changed),
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x=makeCacheMatrix(matrix(4:7, 2, 2)),...) {
    
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
      message("getting cached data")
      return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}

