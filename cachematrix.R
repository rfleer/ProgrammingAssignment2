## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some benefits to changing
## the inverse of a matrix rather than compute it repeatedly.
## Below we find a pair of functions that are used to create a special object that stores a
## matrix and caches its inverse.
  

## This function creates a special "matrix" object that can cache its inverse. 
## The function returns a list of four functions.

makeCacheMatrix <- function(x = matrix()) {
  
  # holds the cached value or NULL if nothing is cached
  # initially nothing is cached so set it to NULL
  inv <- NULL
  
  #FUNCTION 1: store a matrix
  set <- function(y) {
    x <<- y
    # since the matrix is assigned a new value, set cache to NULL again
    inv <<- NULL
  }
  
  #FUNCTION 2: return the stored matrix
  get <- function() x
  
  #FUNCTION 3: cache the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  #FUNCTION 4: return the inverser
  getInverse <- function() inv
  
  #put all the functions in the final list:
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}




## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it retrievs the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    ## Use function 4 in list x to get the inverser
    inv <- x$getInverse()
    
    ## if the inverse already exists (is not NULL) print a message to the console
    ## and return the existing inverse
    if (!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    
    ## if the inverse does not already exist
    ## use function 2 of list x to get the matrix from which to calculate the inverse
    mat <- x$get()
    
    ## calculate the inverser of this matrix
    inv <- solve(mat, ...)
    
    ## and use function 3 of list x to cache the inverse
    x$setInverse(inv)
    
    ## print the inverse to the console
    inv
}