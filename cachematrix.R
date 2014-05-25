## This file contains two functions used define the set of functions that use the <<- operator 
## to assign the calculated inverse matrix value to an object in an environment that is different 
## from the current environment. Below are two functions that are used to create a special object 
## that stores a numeric matrix and caches the calcualted inverse value.

## The function makeCacheMatrix creates a special "matrix", which is actually a list 
## containing the following functions:
## 
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the matrix inverse
## 4) get the value of the matrix inverse
##

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
               x <<- y
               m <<- NULL
      }
      
      get <- function() x
      
      setsolve <- function(solve) m <<- solve
      
      getsolve <- function() m
      ## 
      list(set = set, 
           get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}


## The function cacheSolve calculates the inverse of special matrix created with the function
## makeCacheMatrix function. The function first checks if the matrix inverse has been previously 
## been calcualted. If the matrix inverse does not already exist in the cache then the inverse of 
## the matrix is calculated, added to the cache using the setsolve function, and returned as the 
## result. Otherwise the previosly calculated inverse matrix is returned
## 

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached matrix")
        return(m)
    }
    data <- x$get()
    m <- try(solve(data, ...))
    x$setsolve(m)
    
    ## Return a matrix that is the inverse of 'x'
    m        
}
