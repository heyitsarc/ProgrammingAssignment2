makeCacheMatrix <- function(x = numeric()) {

  # To initialize, nothing is cached.
  cache <- NULL
  
  # Here, the function stores a matrix and remove the cache.
  setMat <- function(newValue) {
    x <<- newValue
    cache <<- NULL
  }
  
  # Here, the function returns the stored matrix.
  getMat <- function() {
    x
  }
  
  # Here, the function cache the given argument.
  cacheInv <- function(solve) {
    cache <<- solve
  }
  
  # Here, the function returns the cached value.
  getInv <- function() {
    cache
  }
  
  # Here, makeCacheMatrix() function returns a list, each named element of the list is a function
  list(set_matrix = setMat, 
       get_matrix = getMat, 
       cache_inverse = cacheInv, 
       get_inverse = getInv
       )
}


# Here, function calculates the inverse of a "special" matrix created with makeCacheMatrix() function.
cacheSolve <- function(y, ...) {
  # It returns the cached value
  inv <- y$get_inverse()
  
  # If a cached value exists, return it.
  if(!is.null(inv)) {
    message("Getting the cached data: Please wait.")
    return(inv)
  }
  
  # If not, it gets the matrix, compute for the inverse and store it in the cache.
  data <- y$get_matrix()
  inv <- solve(data)
  y$cacheInverse(inv)
  
  # Then, return the inverse.
  inv
}
