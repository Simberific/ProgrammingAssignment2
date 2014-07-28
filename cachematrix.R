## Below are two functions, makeCacheMatrix and cacheSolve, which together allow for caching of the inverse of a matrix, so that you don't have to solve for the inverse (which is expensive) if you have already computed this.


## makeCacheMatrix creates a matrix object which has functions allowing the user to get the value of the matrix itself, set it, get the "cached" inverse of the matrix, and set the cached inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ## Sets inverse to NULL when you first make the cached entry
  
  ## Define a set function which sets the value of the vector and re-sets cached inverse to NULL (since you've changed the vector, the previous inverse is no longer valid)
  set <- function(y) {
    x <<- y
    inv <<- NULL  
  }
  get <- function() x ## get function returns the value of the vector
  setinverse <- function(i) inv <<- i ## Sets the cached value of the inverse of x to i
  getinverse <- function() inv ## Returns the value of the cached inverse
  
  
  ## Return the four functions as elements of a list
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## cacheSolve checks whether the matrix's inverse has already been computed, and if so, retrieves it; if not, computes it, caches it to the object as inv, and returns it.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    ## Then you just return the inverse from cache
    message("getting cached data")
    return(inv)
  }
  ## Else, you compute the inverse
  data <- x$get()
  newInv <- solve(data, ...)
  x$setinverse(newInv) ## Set x's inverse to the newly computed inverse
  newInv
  
}
