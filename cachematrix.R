## This functions cache the inverse of a Matrix. The aim is to reduce the cost of computation, 
#  if the contents of a matrix are not changing the cacheSolve function  looks up in the cache before recomputed


## The  makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## Tips: the double left arrow << indicates that the assignment should be made to the parent environment

makeCacheMatrix <- function(x = matrix()) {
            s <- NULL
            set <- function(y= matrix()) {
                  x <<- y
                  s <<- NULL
            }
            get <- function() x
            setsolve <- function(solve) s <<- solve
            getsolve <- function() s
            list(set = set, get = get,
                 setsolve = setsolve,
                 getsolve = getsolve)
      
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## The if condition helps to avoid time consuming.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      s <- x$getsolve()
      if(!is.null(s)) {
            message("getting cached data")
            return(s)
      }
      data <- x$get()
      s <- solve(data, ...)
      x$setsolve(s)
      s
}
