## Put comments here that give an overall description of what your
## functions do

## a special kind of matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
		z <- NULL
	  set <- function(y){
              x <<- y
			  z <<- NULL
      }
	  get <- function()x
	  setsolve <- function(solve) z <<-solve
	  getsolve <- function()z
	  list(set = set, get = get,
	        setsolve = setsolve,
			getsolve = getsolve)
}

## a kind of function that calculates the matrix inverse if the matrix has 
## not been cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          z <- x$getsolve()
		 if(!is.null(z)) {
		         message("getting cached data")
				 return(z)
		 }
         data <- x$get()
		 z <- solve(data, ...)
		 x$setsolve(z)
		 z
}