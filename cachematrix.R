# these two functions will create a special matrix that can cache its inverse.
##Cachesolve will then compute the cached inverse of the matrix or create it if it does not exist

## makecacheMatrix will create a special type of matrix that can cache its inverse
## the function cotains other functions to return or store the inverse

makeCacheMatrix <- function(x = matrix()) {
  ## this is the variable holding the inverse matrix. its default is null
  invMatrix<-NULL;
  ##the set function assigns matrix y to matrix x and resets the inverse matrix
  set <- function(y) {
    x<<-y
    invMatrix<<-NULL
  }
  ## get() returns the matrix x
  get <- function() x
  ##setInv sets the value of inVMatrix
  setInv <- function(inv) invMatrix <<- inv
  ##getInv returns the value of invMatrix
  getInv <- function() invMatrix
  
  list(set = set, get = get,setInv = setInv,getInv=getInv)
  
}


## CacheSolve will return the cached value of matrix x if it has been calculated before
##otherwise, it will calculate the inverse of x and store it in invMatrix

cacheSolve <- function(x, ...) {
 ##get the cached value of inverse matrix
   tempInv <- x$getInv()
   ##if the value is cached display a message and return cached value
   if(!is.null(tempInv)) {
     message("getting cached data")
     return(tempInv)
   }
   ##if the inverse matrix of x is not cached calculate it
   data <- x$get()
   #calculate the inverse of data
   tempInv<-solve(data)
   ##cache the inverse matrix for future use
   x$setInv(tempInv)
   tempInv
  
}
