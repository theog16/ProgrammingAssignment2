## -------------------------------------------------------------------------
## Put comments here that give an overall description of what your
## functions do
##
## Functions which are able to cache potentially time-consuming computations
## such as the inverse of a matrix. Matrix inversion is usually a costly 
## computation and there may be some benefit to caching the inverse of a 
## matrix rather than compute it repeatedly (e.g. in a loop). 
## If the contents of a matrix are not changing,
## it may make sense to cache the value of the inverse so that when we need 
## it again, it can be looked up in the cache rather than recomputed.
##
## They assume that the matrix supplied is always invertible.
## (Computing the inverse of a square matrix can be done with the 
## solve function in R. For example, if X is a square invertible 
## matrix, then solve(X) returns its inverse).
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------
## EXAMPLE 1
## -------------------------------------------------------------------------
## my_matrix <- makeCacheMatrix(rbind(c(1, -1/4), c(-1/4, 1)))
## 
##  my_matrix$get()
## [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00
## 
## my_matrix1$getInverse()
## NULL
## 
## my_matrix$setInverse(cacheSolve(my_matrix))
## 
## my_matrix$getInverse()
## [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## 
#### RE-DEFINE THE MATRIX my_matrix:
## my_matrix$set(rbind(c(2, -1/2), c(-1/2, 2)))
## 
## my_matrix$get()
## [,1] [,2]
## [1,]  2.0 -0.5
## [2,] -0.5  2.0
## 
#### NO INVERSE MATRIX TO CACHE SINCE WE GOT A NEW MATRIX
## my_matrix$getInverse()
## NULL
## 
## my_matrix$setInverse(cacheSolve(my_matrix))
## 
## my_matrix$getInverse()
## [,1]      [,2]
## [1,] 0.5333333 0.1333333
## [2,] 0.1333333 0.5333333
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------
## Write a short comment describing this function
##
## makeCacheMatrix: 
## This function creates a special "matrix" object that can cache its 
## inverse.
##
## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
## -------------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
    inverseMat <- NULL
  set <- function(y) {
    x <<- y
    inverseMat <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inverseMat <<- solve
  getInverse <- function() inverseMat
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## -------------------------------------------------------------------------
## Write a short comment describing this function
##
## cacheSolve: 
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
##
## The following function calculates the inverse of the special "matrix" 
## created with the above function. However, it first checks to see 
## if the inverse has already been calculated. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, it calculates 
## the inverse of the matrix and sets the value of the inverse in the 
## cache via the setinverse function.
## -------------------------------------------------------------------------

cacheSolve <- function(x, ...) {
## cachemean <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverseMat <- x$getInverse()
  if(!is.null(inverseMat)) {
    message("getting cached data")
    return(inverseMat)
  }
  data <- x$get()
  inverseMat <- solve(data, ...)
  x$setInverse(inverseMat)
  inverseMat
}
