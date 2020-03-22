## This function iniates a matrix object with data provide. It creates list of 4 functions which can be called later by names

makeCacheMatrix <- function(x = matrix()) 
  {
  m <- NULL
  set <- function(y) 
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,setinv = setinv,getinv = getinv)
  }


## This function retrieves cached value of inverse of matrix if presenet. If not present then it calculates it for the first time and return, stores it

cacheSolve <- function(x, ...) 
  {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) 
  {
    message("getting cached data")
    return(m)
  }
  # if not cached then calculate
  data <- x$get()
  m <- solve(data, ...)   # solve function finds the inverse of a matrix
  x$setinv(m)
  m
}
