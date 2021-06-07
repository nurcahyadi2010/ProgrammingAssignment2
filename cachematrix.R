rm(list = ls())
## Put comments here that give an overall description of what your
## functions do

#####################################################
## FUNCTION makeCacheMatrix
#####################################################
## It creates matrix and its inverse.
## The function returns a list produced by the 
## following four functions:
## 1. setmat(y): initiate the matrix and set the 
##               inverse to NULL.
## 2. getmat(): get the matrix.
## 3. setinverse(y): calculate inverse from the
##                   available matrix .
## 4. getinverse(): get the matrix inverse.
######################################################
makeCacheMatrix <- function(x = matrix()) {
     ## initiate 'iv' the matrix inverse value as NULL
     iv <- NULL
     
     ## Function to set the matrix
     setmat <- function(y){
          x <<- y
          iv <<- NULL
     }
     
     ## Function to get the matrix
     getmat <- function() x
     
     ## Function for calculating the matrix inverse
     setinverse <- function(y) iv <<- solve(y)
     
     ## Function for getting the matrix inverse
     getinverse <- function() iv
     
     ## Return this list
     list(setmat = setmat, 
          getmat = getmat,
          setinverse = setinverse,
          getinverse = getinverse)

}

## Testing the makeCacheMatrix function

## Create a 3x3 matrix 
m1 <- matrix(data = c(3,2,5,
                      2,3,2,
                      5,2,4),
            nrow = 3, ncol = 3,
            byrow = TRUE)
print(m1)
## Now put matrix m1 in the makeCacheMatrix function
a <- makeCacheMatrix(m1)
a$getmat()
a$getinverse()
a$setinverse(a$getmat())
a$getinverse()



#####################################################
## FUNCTION cacheSolve
#####################################################
## It evaluates the inverse of special matrix from
## 'makeCacheMatrix' function.
## The function will take the cached inverse if it is
## available, otherwise the function will calculate
## the inverse.
######################################################
cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     
     ## Check the availability of matrix inverse
     iv <- x$getinverse()
     if(!is.null(iv)){
          message("Getting cached matrix inverse")
          ## return iv and exit from cacheSolve function
          return(iv)
     }
     ## Otherwise, if the matrix inverse doesn't exist,
     ## calculate it using solve() function.
     message("Calculating matrix inverse")
     iv <- solve(x$getmat())
     iv
}

## Testing the cacheSolve function
a <- makeCacheMatrix(m1)
a$setinverse(a$getmat())
cacheSolve(a)

a <- makeCacheMatrix(m1)
cacheSolve(a)
