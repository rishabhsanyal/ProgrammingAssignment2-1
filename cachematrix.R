## AUTHOR: RISHABH SANYAL 

## Below are the two functions which describes the caching of the inverse of a matrix 
## which is time consuming since it occurs repeatedly.

## The fisrt function creates a matrix object which is used to cache & stores the result of inverse 
##  of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  inverse_var <- NULL
  
  set <- function(y) {
    
    x <<- y
    inverse_var<- NULL
    
  }
  
  get  <- function() x
    
    setinverse <- function(inverse) inverse_var <<- inverse
    
    getinverse <- function() inverse_var
    
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  }




## This following function computes the inverse of the matrix object created by above function
## If the inverse of the matrix is already calculated & the matrix values are not changed therefore
## this function goes to cache and fetches the matrix inverse values from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv_second <- x$getinverse()
  if (!is.null(inv))
  {
    message(" Start getting the matrix values from CACHE")
    return (inv_second)
  }
  
  mat<-x$get()
  inv_second<- solve (mat, ...)
  x$setinverse
  inv_second
}

