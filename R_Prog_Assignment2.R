# Assignement
#Assignment: Caching the Inverse of a Matrix

# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.
# 
# Write the following functions:
#   
#   makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
# Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.
# 
# For this assignment, assume that the matrix supplied is always invertible.


                  ## Solution Follows below ##
#makeCacheMatrix function creats a matrix object and caches its inverse.

makeCacheMatrix <- function(x = matrix()){
  
  i <- NULL
  set <-  function(y) {
     x <<- y
     i <<- NULL
   }
  get <- function() y
  setinverse <- function(inverse) i <<-inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#cacheSolve function checks if an inverse for given matrix is already available in above function or not(if the matrix is not changed). if an inverse cache is already available it returns the inverse from cached data otherwise it calculates the inverse matrix.
cacheSolve <- function(x = matrix()){
  f<-makeCacheMatrix(y)
  if(sum(x - f$get())== 0) i <- f$getinverse() else i <- NULL;#validating if the matrix is changed or not.
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  matrix <- f$get()
  i <- solve(matrix)
  f$setinverse(i)
  message("new value")
  i
}

