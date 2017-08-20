#-----------------
# MatrixInverse
#-----------------
# Author: Jeremy Sidwell
# Date: August 19, 2017
#-------------------------
#
# This function calculates the inverse of a 
# matrix using the solve function.  However, 
# this function checks for two conditions before
# computing: 
# 1) The matrix is an nxn matrix
# 2) The determinant is non-zero
#
# If either of these conditions do not hold,
# the function creates a default 2x2 matrix.
# 
#-----------------------------------------------
MatrixInverse <- function (x=matrix()) {
   if(det(x) != 0 && (nrow(x) == ncol(x))) {solve(x)}
   else {
     x<- matrix(c(1, 2, 
                  3, 4), nrow=2, byrow=TRUE)
     
     print("Input matrix is not invertable. Setting input to the following 2x2 matrix:")
     print(x)
     solve(x)
   }
}

#-----------------
# makeCacheMatrix
#-----------------
# Author: Jeremy Sidwell
# Date: August 19, 2017
#-------------------------
#
# This function creates a special "matrix"
# object that can cache its inverse.
#
# Various Steps to creates a special "matric", 
# which is really a list containing a function to:
# 1. Set the matrix
# 2. Get the matrix
# 3. Set the inverse of the matrix
# 4. Get the inverse of the matrix
# 
#-----------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(MatrixInverse #Inverse matrix funtion
                        ) m <<- MatrixInverse # Inverse matrix function
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#-----------------
# cacheSolve
#-----------------
# Author: Jeremy Sidwell
# Date: August 19, 2017
#-------------------------
#
# This function computes the inverse of the 
# special "matrix" returned by makeCacheMatrix. 
# If the inverse has already been 
# calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the 
# inverse from the cache.
#
#-----------------------------------------------
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- MatrixInverse(data, ...)
  x$setinverse(m)
  m
}

#---------------------
# Testing Cache Matrix
# ---------------------

mcmat <-makeCacheMatrix()
A <- matrix( c(1, 2, 
               3, 4), nrow=2, byrow=TRUE)
B <- matrix( c(3, 9, -4,
               1,-2, 6,
               0, 7,-5), nrow=3, byrow=TRUE)
C <- matrix( c(6, 3, 
               4, 2), nrow=2, byrow=TRUE)



det(A)
solve(A)
MatrixInverse(A)
mcmat$set(A)
mcmat$get()
mcmat$setinverse(MatrixInverse(A))
mcmat$getinverse()
cacheSolve(mcmat)


det(B)
solve(B)
MatrixInverse(B)
mcmat$set(B)
mcmat$get()
mcmat$setinverse(MatrixInverse(B))
mcmat$getinverse()
cacheSolve(mcmat)

solve(C)
det(C)
MatrixInverse(C)
mcmat$set(C)
mcmat$get()
mcmat$setinverse(MatrixInverse(C))
mcmat$getinverse()
cacheSolve(mcmat)
