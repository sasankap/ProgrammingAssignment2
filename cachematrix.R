##makeCacheMatrix: This function creates a special "matrix" object that can 
##cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
##Assumes matrix supplied are invertible



makeCacheMatrix <- function(x = matrix()) {
  #set value of matrix
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #get value of matrix
  get <- function() x
  
  #set inverse of matrix
  setinv <- function(solve) m <<- solve
  
  #get inverse of matrix
  getinv <- function() m
  
  #return matrix with functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {

  m <- x$getinv()
  
  #if inverse is already calculated return it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # no inverse in cache, so create it
  data <- x$get()
  m <- solve(data, ...)
  
  #cache it
  x$setinv(m)
  
  #return it
  m
}
