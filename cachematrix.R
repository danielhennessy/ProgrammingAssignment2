## this function creates a "matrix' object that can cache
## its inverse. 

# the objects functions can 
#1. set the value of the matrix
#2. get/retrive the value of the matirx
#3. set the value of the inverse of the matrix
#4. get/retrieve the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  inverse <- NULL
  
  ##---------------------------------------
  ## set new matrix
  setmatrix <- function(y) {
    x<<- y
    
    ## initialize inverse matrix value as new matrix value set
    inverse <<- NULL
  }
  
  ##---------------------------------------
  ## gets/returns the value of a matrix
  getmatrix = function() 
      return(x)
  
  ##---------------------------------------
  ## sets the value of the inverse matrix
  setinverse <- function(solve)  {
    inverse <<- solve
  }
  
  ##---------------------------------------
  ## gets the value of the inverse matrix
  getinverse <- function() 
      return(inverse)
  
  ##---------------------------------------
  ## returns a list with functions
  list(setmatrix=setmatrix, getmatrix=getmatrix, setinverse=setinverse, getinverse=getinverse)
  
}


## This function retrieves the cache matrix inverse, if it exists then it 
## return the value otherwise is will calcualte the inverse using the 'solve'
## function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  
  s <- x$getinverse()
  data <- x$getmatrix()
  
  ## check if cached data already exists i.e. s ne NULL
  if(!is.null(s)) {
     
      message("getting cached data")
      return(s)

  }

## if data changed or cache not available then get inverse of matrix
##    data <- x$getmatrix()
    s <- solve(data, ...)
    x$setmatrix(x) 
    x$setinverse(s)
    
    ## return inverse
    return(s)

}
