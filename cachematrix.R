## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function 

###makeCacheMatrix stores the original matrix (x) into the cache and set up the setinv and getinv functions,
###will later be retrievable from the cache.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL #default if 1st time running cacheSolve
  y<-NULL #default if 1st time running cacheSolve
  set <- function(y) { #set up matrix 
      x<<- y #cache value of input matrix
      m<<-NULL #sets inverse matrix to NULL to indicate data is cached.
      
  }
  get <- function() x  
  setinv <- function(inverse) m<<-inverse
  getinv <- function () m
  list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getinv() #retrieves "m" from cache. If cacheSolve was run before, then m <- NULL
  ##If m <-null, then !is.null(m) will returne "TRUE", indicating cacheSolve was run previously and 
  ## the solution is in the cache.

    if(!is.null(m)) {
      message("getting cached data")
      return (m) #returns cached value of m
      }
      data <- x$get() #if m was not = null, then data is set to the input matrix (x), pulled from matrix x stored in cache.
      m <- solve(data, ...) #m = inverse matrix / solve the matrix for the inverse.
      x$setinv(m) #store inverse matrix in cache for future reference.
      m #print inverse matrix
  }
  
  
  
  
        ## Return a matrix that is the inverse of 'x'
