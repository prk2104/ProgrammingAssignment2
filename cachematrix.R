###  Below are two functions that are used to create a special object that stores a square
##  invertible matrix and cache's its inverse.

##  The first function, makeVector creates a special "vector", which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse



makeCacheMatrix<- function(x = matrix()) {          ##define the first function
  i <- NULL                                         ##reserve a variable for the calculated inverse 
  set <- function(y) {                              ## define a function to set the value of the matrix
    x <<- y                                         ## set function stores x,i in parent environment
    i <<- NULL
  }
  get <- function() x                               ## get function returns x   
  setinverse <- function(inverse) i <<- inverse     ## setinverse function sets value of inverse from global env.
  getinverse <- function() i                        ## getinverse function gets the vale of the inverse
  list(set = set, get = get,                        ## our main munction returns a list of 4 functions
       setinverse = setinverse,
       getinverse = getinverse)
}
cacheSolve <- function(x, ...) {                    ## define the second function; takes output of first function
  i <- x$getinverse()                               ## store inverse to i
  if(!is.null(i)) {                                 ## if i is not null, get from cache
    message("getting cached data")
    return(i)
  }
  data <- x$get()                                   ## otherwise; store value of matrix
  i <- solve(data, ...)                             ## and calculate its inverse
  x$setinverse(i)                                   ## store tyhe inverse for next run and 
  i                                                 ##and return i, the inverse         
}
