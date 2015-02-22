## Put comments here that give an overall description of what your
## functions do

## Overall, the makeCacheMatrix and the cacheSolve functions create a 'global' environment
## where the inverse of matrices can be calculated and the result of each calculation
## can be kept so that if the same matrix needs inverting again, repeating the calculation 
## will not be necessary.  Instead, the cacheSolve tries to find the inverses it has
## already calculated and return those.

## The functions make use of a special assignmengt operator "<<-" which assigns a value
## to a variable in the context outside of the calling function.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  
  ## Return a list made up of the following elements:
  ##      $set which is function to set the 'global' x value
  ##      $get which is a function that just returns the global value
  ##      $setinv records the calculated inverse of the given matrix
  ##      $getinv returns the calculated inverse of a given matrix (if exists)
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## First, look in the list that was prepared for x using makeCacheMatrix
  ## and try to get the $getinv element of this list (which would be the cached inverse)
  m <- x$getinv()
  
  ## If it was possible to find the cached inverse (i.e m is not NULL)
  if(!is.null(m)) {
    ## Let the user know that the result being returned is from the cache
    message("getting cached inverse")
    ## Return the cached inverse and exit the function 
    ## Note: no calculation of the inverse is necessary
    return(m)
  }
  
  ## If we reach this point, then the cached inverse was not found, so it needs
  ## to be calculated
  
  ## Use the portion of the list that is the matrix itself
  data <- x$get()
  
  ## Calculate the inverse of the matrix
  m <- solve(data, ...)
  
  ## Put the result of the calculation in the global list for future use
  x$setinv(m)
  
  ## Return the inverse
  m
}