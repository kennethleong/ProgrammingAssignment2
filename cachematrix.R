
#These functions are created to cache the inverse of a matrix,
# rather than compute it repeatedly as Matrix inversion is usually a costly computation.
#=======================================================================================

#makeCacheMatrix makes/get ready some methods/functions for a matrix to cache and retrieve its inverse.
#The methods/functions are:
#set - cache a matrix 
#get - retrieve the matrix
#setinverse - cache the inverse of the matrix
#getinverse - retrieve the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  set <- function(y) { 
    x <<- y 
    i <<- NULL 
  }
  get <- function() x 
  setinverse <- function(inverse) i <<- inverse 
  getinverse <- function() i 
  
  list(set = set, get = get, 
    setinverse = setinverse, 
    getinverse = getinverse) 
  
}

#cacheSolve computes the inverse of a matrix and caches it.
#When a matrix has its inverse cached, the cached inverse will be returned rather than needs to recompute.

cacheSolve <- function(x, ...) {
  
  #Retrieve, check and return the cached inverse of a matrix if any.
  i <- x$getinverse()
  if(!is.null(i)) { 
    message("getting cached data") 
    return(i)
  }
  
  #If there is no cached value yet, computes, caches and returns the inverse of the matrix
  data <- x$get() 
  i <- solve(data, ...) 
  x$setinverse(i) 
  i
  
} 
