## R Programming Assignment 2 from Coursera Data Science course path
## Calculate the inverse of an invertible matrix and cache the result for reuse
## Author: Cristina Mandras
## Date: Sep 21, 2014

## How to use these functions?
## 1. Assume you have a square matrix x 
## 2. j<-makeCacheMatrix(x)
## 3. cacheSolve(j) 
## Note that if you use cacheSolve(makeCacheMatrix(x)) the value doesn't get 
## cached, instead it is calculated every time.

## Create a list of functions:
## 1. Set the value of the matrix for which we calculate the inverse
## 2. Get the value of the above matrix
## 3. Set the value of the inverted matrix (cached in the environment)
## 4. Get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  mInv <- NULL
  setM <- function(y) {
    x <<- y
    mInv <<- NULL
  }
  getM <- function() x
  setInv <- function(solve) mInv <<- solve
  getInv <- function() mInv
  list(setM = setM, getM = getM,
       setInv = setInv,
       getInv = getInv)
}


## Check if the inverted matrix has already been calculated.
## If yes, retrieve from cache. If not, calculate it.
## You must first call the makeCacheMatrix function above, see the "How to use"
## section at the beginning.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mInv <- x$getInv()
  if(!is.null(mInv)) {
    message("Getting cached data")
    return(mInv)
  }
  data <- x$getM()
  mInv <- solve(data)
  x$setInv(mInv)
  message("Calculating data")
  mInv
  
}
