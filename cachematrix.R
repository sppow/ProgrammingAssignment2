#A pair of functions that cache the inverse of a matrix:
#makeCacheMatrix $ cacheSolve

#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) 
{
  temp <- NULL
  set <- function(y) 
  {
    x <<- y
    temp <<- NULL
  }
  get <- function() 
  {
    x
  }
  setInverse <- function(inverse) 
  {
    temp <<- inverse
  }
  getInverse <- function() 
  {
    temp
  }
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

#This function computes the inverse of the special "matrix" returned by 
#makeCacheMatrix above. If the inverse has already been calculated (and the 
#matrix has not changed), then cacheSolve should retrieve the inverse from 
#the cache.
cacheSolve <- function(x, ...) 
{
  #Return a matrix that is the inverse of 'x'
  temp <- x$getInverse()
  if (!is.null(temp)) 
  {
    message("getting cached matrix")
    return(temp)
  }
  data <- x$get()
  temp <- solve(data, ...)
  x$setInverse(temp)
  temp
}